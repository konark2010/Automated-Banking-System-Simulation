-module(money).
-export([start/1, spawnBankProcesses/1, spawnCustomerProcesses/2, transactionLogMessagePrinter/4, getInitialLoanSeekingAmount/2, bankInfoPrinter/3]).

% initial start point of the file which accepts the input files as argument
start(Args) ->
    % reads the bank and customer files information and store them in a list
    CustomerFile = lists:nth(1, Args),
    BankFile = lists:nth(2, Args),
    {ok, CustomerInfo} = file:consult(CustomerFile),
    {ok, BankInfo} = file:consult(BankFile),
    CustomerLists = CustomerInfo,
    BankLists = BankInfo,
    io:format("** The financial market is opening for the day ** ~n"),
    io:format("Starting transaction log...... ~n"),
    io:format("~n"),
    % registers itself for being called for transaction log printing
    register(masterProcess, self()),
    spawnBankProcesses(BankLists),
    timer:sleep(500),
    spawnCustomerProcesses(CustomerLists, BankLists),
    transactionLogMessagePrinter(CustomerLists, BankLists, [], []).

% method to spawn bank processes from bank lists and also register them
spawnBankProcesses(BankLists) ->
    lists:foreach(
        fun({Key, Value}) ->
            register(Key, spawn(bank, bankMasterProcess, [Key, Value]))
        end,
        BankLists
    ).

% method to spawn customer processes from customer lists and also register them
spawnCustomerProcesses(CustomerLists, BankLists) ->
    lists:foreach(
        fun({Key, Value}) ->
            register(Key, spawn(customer, customerMasterProcess, [Key, Value, BankLists]))
        end,
        CustomerLists
    ).

% method to handle fetching of the initial loan objective of the customer while printing customer info
getInitialLoanSeekingAmount(OldCustomerLists, Customer) ->
    CustomerMap = maps:from_list(OldCustomerLists),
    case maps:is_key(Customer, CustomerMap) of
        true ->
            maps:get(Customer, CustomerMap);
        false ->
            0 % Default initial value if the key is not found in the map
    end.

% method to print bank final info
bankInfoPrinter(OldBankLists, NewBankLists, Counter) ->
    Terminator = length(OldBankLists) + 1,

    case Counter == Terminator of
        true ->
            TotalOriginal = lists:sum([Value || {_, Value} <- OldBankLists]),
            TotalLoaned = TotalOriginal - lists:sum([Value || {_, Value} <- NewBankLists]),
            io:format("-----~n"),
            io:format("Total: original ~p, loaned ~p~n", [TotalOriginal, TotalLoaned]);
        false ->
            {Bank, BankMoneyResource} = lists:nth(Counter, OldBankLists),
            BankPid = whereis(Bank),
            BankPid ! {self(), "Send current Money Resource"},
            receive
                {Bank, Amount} ->
                    io:format("~p: original ~p, balance ~p~n", [Bank, BankMoneyResource, Amount]),
                    NewList = [{Bank, Amount} | NewBankLists],
                    bankInfoPrinter(OldBankLists, NewList, Counter + 1)
            end
    end.

% method to print transaction logs
transactionLogMessagePrinter(OldCustomerLists, OldBankLists, NewCustomerLists, NewBankLists) ->
    case length(OldCustomerLists) == length(NewCustomerLists) of
        true ->
            % If this condition is satisfied, we print the banking report with customer info
            io:format("~n~n"),
            io:format("** Banking Report **~n~n"),
            io:format("Customers:~n"),

            lists:foreach(
                fun({Customer, InitialAmount}) ->
                    ReceivedAmount = proplists:get_value(Customer, NewCustomerLists, -1),
                    io:format("~p: objective ~p, received ~p~n", [Customer, InitialAmount, ReceivedAmount])
                end,
                OldCustomerLists
            ),
            % Logic to print the total objective and received amounts of the customers
            TotalObjective = lists:sum([Value || {_, Value} <- OldCustomerLists]),
            TotalReceived = lists:sum([Value || {_, Value} <- NewCustomerLists]),
            io:format("-----~n"),
            io:format("Total: objective ~p, received ~p~n", [TotalObjective, TotalReceived]),
            io:format("~n~nBanks:~n~n"),
            bankInfoPrinter(OldBankLists, [], 1);

        false ->
            receive
                % Transaction message handler
                {TransactionLog} ->
                    io:format("~s ~n", [TransactionLog]),
                    transactionLogMessagePrinter(OldCustomerLists, OldBankLists, NewCustomerLists, NewBankLists);
                % Message handler for printing customer info
                {TransactionLog, Customer, FinalAmountSeeking} ->
                    % Gets the initial loan seeking value of the customer 
                    InitialLoanSeekingValue = getInitialLoanSeekingAmount(OldCustomerLists, Customer),
                    TotalAmountReceived = InitialLoanSeekingValue - FinalAmountSeeking,
                    CustomerTuple = {Customer, TotalAmountReceived},
                    CustomerList = [CustomerTuple | NewCustomerLists],
                    transactionLogMessagePrinter(OldCustomerLists, OldBankLists, CustomerList, NewBankLists)
            end
    end.

