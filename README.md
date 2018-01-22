# Predict PM10 as functions of time

Joint work with [Jean-Michel Poggi](http://www.math.u-psud.fr/~poggi/) and [Bruno Portier](http://lmi2.insa-rouen.fr/~bportier/)

---

Forecast a curve sampled within the day (seconds, minutes, hours...),
using past measured curves + past exogenous informations, which could be some aggregated
measure on the past curves, the weather... Main starting point: computeForecast().

NOTE: algorithms are not specific to PM10 and could be applied to anything else (in similar contexts).
However, seasons are hard-coded to follow pollution events.
You may want to change them in pkg/R/utils.R, function .isSameSeason()

NOTE 2: the package works for only one series (one station). Further extensions might consider the multi-series case.

---

The final report may be found at [this location](http://www.airnormand.fr/Publications/Publications-telechargeables/Rapports-d-etudes)
