"use strict";

var global_vars = {}
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
        if (ts) {
            global_vars.ts = ts;
            renderTransactions(ts);
        }
    }
    req.open("get", "/transactions", true);
    req.send();
}

function addToRow(row, text) {
    var td = document.createElement("td");
    td.appendChild(document.createTextNode(text));
    row.appendChild(td);
}

function renderTransactions(ts) {
    var root = document.getElementById("transactions");
    for (var i in ts) {
        var t = ts[i];
        var row = document.createElement("tr");
        addToRow(row, t.date);
        addToRow(row, t.description);
        addToRow(row, t.amount/100);
        addToRow(row, JSON.stringify(t.tags));
        transactions.appendChild(row);
    }
}

function error(e) {
    document.getElementById("error").innerHtml = "" + e;
}

function dopost() {
    var req = new XMLHttpRequest();
    req.onload = function() {
        alert(this.responseText);
    }
    req.open("post", "/similar", true);
    req.send(JSON.stringify(global_vars.ts[10]));
}