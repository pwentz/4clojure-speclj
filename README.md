# four-clojure

Problems from [4clojure](https://www.4clojure.com/), tested with [Speclj](http://speclj.com/).

For when you want to do some 4clojure problems, but don't want to write the code in the browser.

## Adding a problem

If the problem you want to solve isn't tested in ```fourclojure.core-spec``` yet:

* Find the problem on 4clojure
* Add the spec to ```fourclojure.core-spec```, using the number as the ```describe```
  and the description as the ```it```
* Add any restricted functions using ```with-restrictions```
  (Note: functions with ```:inline``` meta, like ```nth```, can't be redeffed, so ```with-restrictions``` may not always work)
* Add the assertions as the ```should```(s), replacing the underscores with a function named as the problem number

## Solving a problem

* ```$ script/restart``` if you want to start from scratch
* Add your function to ```fourclojure.core```
* Run the specs

```bash
$ lein spec -a
```
