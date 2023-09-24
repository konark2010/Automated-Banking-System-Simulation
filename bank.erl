-module(bank).
-export([bankMasterProcess/2]).

% Initial start point of the file which accepts the bank name and money resource from the master process
bankMasterProcess(Bank, BankMoneyResource) ->
    % Retrieves the master process id for sending a message to the master process
    MasterPid = whereis(masterProcess),
    % Starting the listen state to accept messages from the customer process
    receive
        {MasterPid, Message} ->
            MasterPid ! {Bank, BankMoneyResource};
        {CustomerPid, Customer, LoanSeekedAmount} ->
            case LoanSeekedAmount =< BankMoneyResource of
                true ->
                    % Loan approval case if the bank has enough money resource
                    CustomerPid ! {"Approved", Bank},
                    MessageToMasterProcess = "$ The " ++ atom_to_list(Bank) ++ " approves a loan of " ++ integer_to_list(LoanSeekedAmount) ++ " dollar(s) from the " ++ atom_to_list(Customer),
                    MasterPid ! {MessageToMasterProcess},
                    NewBankMoneyResource = BankMoneyResource - LoanSeekedAmount,
                    bankMasterProcess(Bank, NewBankMoneyResource);
                false ->
                    % Loan decline case
                    CustomerPid ! {"Declined", Bank},
                    MessageToMasterProcess = "$ The " ++ atom_to_list(Bank) ++ " denies a loan of " ++ integer_to_list(LoanSeekedAmount) ++ " dollar(s) from the " ++ atom_to_list(Customer),
                    MasterPid ! {MessageToMasterProcess},
                    bankMasterProcess(Bank, BankMoneyResource)
            end
    end.
