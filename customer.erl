-module(customer).
-export([customerMasterProcess/3]).

% Initial start point of the file which accepts the customer name, loan amount required, and bank lists from the master process
customerMasterProcess(Customer, LoanSeekingAmount, BankLists) ->
    MasterPid = whereis(masterProcess),
    BankListsLength = length(BankLists),
    if
        % Checking if the loan seeking amount of the customer is 0 or the bank list is empty
         LoanSeekingAmount == 0 orelse BankListsLength == 0 ->
            MessageToMasterProcess = "Transaction from " ++ atom_to_list(Customer) ++ " finished with " ++ integer_to_list(LoanSeekingAmount),
            MasterPid ! {MessageToMasterProcess, Customer, LoanSeekingAmount};
        % Else condition if the loan seeking amount is not zero or the bank list is not empty
        true ->
            % Selects a random amount to be asked from the bank
            AmountToBeAsked =
                case LoanSeekingAmount < 50 andalso LoanSeekingAmount > 0 of
                    true ->
                        rand:uniform(LoanSeekingAmount);
                    false ->
                        rand:uniform(49) + 1
                end,
            % Selects the random bank from the banks list for loan seeking
            RandomBank = rand:uniform(BankListsLength),
            case lists:nth(RandomBank, BankLists) of
                {Key, Value} ->
                    ProspectedBankPid = whereis(Key),
                    timer:sleep(rand:uniform(41) + 10),
                    ProspectedBankPid ! {self(), Customer, AmountToBeAsked},
                    MessageToMasterProcess = "? " ++ atom_to_list(Customer) ++ " requests a loan of " ++ integer_to_list(AmountToBeAsked) ++ " dollar(s) from the " ++ atom_to_list(Key) ++ " bank",
                    MasterPid ! {MessageToMasterProcess},
                    receive
                        {BankReply, _} ->
                            case BankReply of
                                % Sending an approval message to the master process after bank approval
                                "Approved" ->
                                    customerMasterProcess(Customer, LoanSeekingAmount - AmountToBeAsked, BankLists);
                                % Sending a decline message to the master process after bank denial
                                "Declined" ->
                                    NewBankLists = lists:filter(fun({K, _}) -> K /= Key end, BankLists),
                                    customerMasterProcess(Customer, LoanSeekingAmount, NewBankLists);
                                % Any other case handler
                                _ ->
                                    io:format("String does not match any pattern.~n")
                            end
                    end;
                % Invalid list index case handler
                _ ->
                    io:format("Invalid index or list is empty.~n")
            end
    end.