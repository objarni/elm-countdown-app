# coding: utf-8
import bottle


static_paths = [
    "js/app.js",
    "index.html",
    "bg.jpg",
]


@bottle.route("/<path:re:.*>")
def path(path):
    if path in static_paths:
        response = bottle.static_file(path, ".")
        response.set_header("Cache-Control", "public, max-age=5")
        return response
    else:
        return "Unknown path"


@bottle.route("/")
def root():
    bottle.redirect("/index.html")


bottle.run(
    reloader=True, host="localhost", port=8000,
)
