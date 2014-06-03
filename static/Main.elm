import Json
import JsonUtils (intFromJson, stringFromJson, listFromJson)
import Model (Transaction, transactions)
import View (renderTransactions)

main : Signal Element
main = maybe (asText "ERROR") renderTransactions <~ transactions
