commonStyle = [ ["height", "18px"]
              , ["padding", "0px"]
              , ["margin", "0px"]
              , ["visibility", "visible"]
              , ["pointer-events", "auto"]
              , ["text-align", "left"]
              ];

function fwDiv(w, c, a, s) {
    a = a || [];
    s = s || [];
    return util.html("div", a, [["width", w + "px"], ["float", "left"]].concat(commonStyle).concat(s), c);
}

function wDiv(w, c, a, s) {
    a = a || [];
    s = s || [];
    return util.html("div", a, [["width", w + "px"]].concat(commonStyle).concat(s), c);
}

window.onload = function() {
    function renderNavBar() {
        return wDiv(1920, fwDiv(100, "TList", [['onclick', 'state(["tlist"]);']]));
    }
    $('#navbar').html(renderNavBar());

    util.jsonGet("/transactions", function(ts) {
        TList(ts); // TODO handle null error
        state(["tlist"]);
    });
};

var TList = $R.state(null);
var similarTS = $R.state(null);
var editT = $R.state(null);

var state = $R(function(s) {
    util.clear();
    if (s[0] == "tlist") { util.showPage("transaction_list", renderTList(TList())); }
    else if (s[0] == "edit") {
        util.jsonPost("/similar", editT(), function(a) { similarTS(a); });
        util.showPage("edit", renderEdit(editT(), similarTS()));
    }
});

var renderTList = function(ts) {
    function showAmount(a) {
        return (a/100) + "";
    }

    function balance(a) {
        return _.foldl(a, function(s, e) { return s + e.amount; }, 0);
    }

    // renderedts :: [String]
    var renderedts = _.map(ts, function(t) {
        return wDiv(1920, _.concat([ fwDiv(100, t.date)
                                   , fwDiv(1000, t.description)
                                   , fwDiv(100, showAmount(t.amount))
                                   , fwDiv(100, JSON.stringify(t.tags), [['onclick', 'trTagClick("' + btoa(JSON.stringify(t)) + '")']], [['cursor', 'pointer']])
                                   ]));
    });
    return renderedts;
};
$R(function(ts) {
    $('#transaction_list').html(renderTList(ts));
}).bindTo(TList);

var renderEdit = function(t, ts) {
    function showAmount(a) {
        return (a/100) + "";
    }

    // renderedts :: [String]
    var renderedts = _.map(ts, function(a) {
        var t = a[1];
        return wDiv(1920, _.concat([ fwDiv(100, t.date)
                                   , fwDiv(1000, t.description)
                                   , fwDiv(100, showAmount(t.amount))
                                   , fwDiv(100, JSON.stringify(t.tags))
                                   , fwDiv(100, a[0])
                                   ]));
    });
    return util.html("div", [], [], JSON.stringify(t)) + util.html("div", [], [], _.concat(renderedts));
};

$R(function(t, ts) {
    $('#edit').html(renderEdit(t, ts));
}).bindTo(editT, similarTS);

var trTagClick = util.jsonFunc(function(t) {
    editT(t);
    state(["edit"]);
});
