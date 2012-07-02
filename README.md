# Github Events

Web Application for show stream of specific github events.

## Run

Requirements:

* [Erlang](http://www.erlang.org/download.html);
* If you on Windows, you need [Cygwin](http://www.cygwin.com/install.html) with `make`.

```
git clone https://github.com/kutu/github-events.git
cd github-events
make
make run
```

Go to [http://localhost:5000](http://localhost:5000)

## Deploy to Heroku

Requirements:

* Done __Run__ section above;
* [Heroku toolbelt](https://toolbelt.heroku.com/).

```
heroku create
heroku config:add BUILDPACK_URL=http://github.com/heroku/heroku-buildpack-erlang.git
git push heroku master
heroku ps:scale web=1
heroku open
```
