function parse_query () {
    // Parse GET query
    var query = {};
    if (1 < window.location.search.length) {
        var str = window.location.search.substring(1); // Remove the first '?'
        var entries = str.split('&');
        for (var i = 0; i < entries.length; i++) {
            var pair = entries[i].split('=');
            var key = decodeURIComponent(pair[0]);
            var val = decodeURIComponent(pair[1].replace(/\+/g, ' '));
            query[key] = val;
        }
    }

    return query;
}

// Loads a JavaScript file and calls k after loading.
function loadjs (fname, k) {
    var elm = document.createElement("script");
    elm.setAttribute("type","text/javascript");
    elm.setAttribute("src", fname);
    elm.onload = k;
    document.getElementsByTagName("head")[0].appendChild(elm);
}

document.body.onload = function () {
    var query = parse_query();

    $("#q").val(query["q"]);

    if (query["q"]) {
        $("#graph").css("background-image", "url(loading.gif)");

        loadjs("js/viz.js", function () { // https://github.com/mdaines/viz.js
            loadjs("js/infer.js", function () {
                // Executed after loading JavaScript files:
                var res = JSON.parse(CRFInfer(query["q"]));
                var dot = res[1];
                var log_potential = res[2];
                var potential = Math.exp(log_potential);
                var z = res[3];
                var svg = Viz(dot, "svg");

                $("#log-potential").html(log_potential);
                $("#potential").html(potential);
                $("#probability").html(potential * z);
                $("#graph").html(svg);
                $("#graph").css("background-image", "none");

                $("#download").attr("href",
                                    "data:image/svg+xml," + encodeURIComponent(svg));
                $("#download").css("display", "inline");
            });
        });
    }
};
