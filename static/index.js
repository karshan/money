"use strict";

main();

function main() {
    var req =new XMLHttpRequest();
    req.onload = function() {
        var ts = null;
        try {
            ts = JSON.parse(this.responseText);
        } catch(e) {
            error(e);
        }
        if (ts) renderTransactions(ts);
    }
    req.open("get", "/transactions", true);
    req.send();
}

function renderTransactions(ts) {
    var root = document.getElementById("transactions");
    for (var i in ts) {
        var t = ts[i];
        var span = document.createElement("div");
        var text = document.createTextNode("Date: " + t.date + " Description: " + t.description + " Amount: " + t.amount + " Running Balance: " + t.runningBalance);
        span.appendChild(text);
        transactions.appendChild(span);
    }
}

function error(e) {
    document.getElementById("error").innerHtml = "" + e;
}
