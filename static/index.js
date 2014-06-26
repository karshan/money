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

function showAmount(a) {
    return (a/100) + "";
}

function showTags(tags) {
    return "[" + _.concat(_.intersperse(tags, ", ")) + "]";
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
    function balance(a) {
        return _.foldl(a, function(s, e) { return s + e.amount; }, 0);
    }

    // renderedts :: String
    var renderedts = _.concat(_.map(ts, function(ts_month) {
        return _.concat(_.map(ts_month, function(t) {
            return wDiv(1920, _.concat([ fwDiv(100, t.date)
                                       , fwDiv(1000, t.description)
                                       , fwDiv(100, showAmount(t.amount))
                                       , fwDiv(100, showTags(t.tags), [['onclick', 'trTagClick("' + btoa(JSON.stringify(t)) + '")']], [['cursor', 'pointer']])
                                       ]));
        })) + util.html("div", [], [], "&nbsp;");
    }));
    return   util.html("div", [], [], "Balance: " + showAmount(balance(_.concat(ts))))
           + util.html("div", [], [], renderedts)
};
$R(function(ts) {
    $('#transaction_list').html(renderTList(ts));
}).bindTo(TList);
var trTagClick = util.jsonFunc(function(t) {
    editT(t);
    state(["edit"]);
});

var renderEdit = function(t, ts) {
    // renderedts :: String
    var renderedts = _.concat(_.map(ts, function(a) {
        var t = a[1];
        return wDiv(1920, _.concat([ fwDiv(100, t.date)
                                   , fwDiv(1000, t.description)
                                   , fwDiv(100, showAmount(t.amount))
                                   , fwDiv(100, "[" + _.concat(_.intersperse(t.tags, ", ")) + "]")
                                   , fwDiv(100, a[0], [['onclick', 'editTrClick("' + btoa(JSON.stringify(t)) + '")']], [['cursor', 'pointer']])
                                   ]));
    }));
    return   util.html("div", [], [], JSON.stringify(t))
           + util.html("div", [], [], renderedts)
           + util.html("input", [["type", "text"], ["id", "edit_tag_in"]], [], "")
           + util.html("button", [['onclick', util.jsonOnclick('editSubmitClick', "")]], [], "submit");
};
$R(function(t, ts) {
    $('#edit').html(renderEdit(t, ts));
}).bindTo(editT, similarTS);
var editTrClick = util.jsonFunc(function(t) {
    similarTS(_.takeWhile(similarTS(), function(a) { return !_.isEqual(a[1], t) }));
});
var editSubmitClick = util.jsonFunc(function() {
    tags = $('#edit_tag_in').val();
    util.jsonPost("/updateTags", [_.map(similarTS(), function(a) { return a[1]; }), tags], function() { alert('done'); });
});
