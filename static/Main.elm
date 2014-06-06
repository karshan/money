import Json
import JsonUtils (intFromJson, stringFromJson, listFromJson)
import Inputs (transactionClicks)
import Model (Transaction, transactions)
import View (renderTransactions, transactionClicks)

main : Signal Element
main = lift (flow down) <| combine [ asText <~ transactionClicks.signal
                                   , (maybe (asText "ERROR") renderTransactions <~ transactions)
                                   ]
