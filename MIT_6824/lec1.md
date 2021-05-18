## Intro

* [MIT 6.824 Spring 2021](https://pdos.csail.mit.edu/6.824/schedule.html)
* [Lecture 1 Video](https://www.youtube.com/watch?v=WtZ7pcRSkOA)

distributed system:

* multiple cooperating computers
* storage for big web sites
* as a backbone of the critical infrastructure

More details refers to [notes\_l01](https://pdos.csail.mit.edu/6.824/notes/l01.txt).

> TODO: save online resources such as notes to local.

main topics:

* fault tolerance: availability, recoverability
* consistency
* performance: scalability

## MapReduce

Overall goal:

* easy for non-specialist programmer of distributed system
* just defines Map and Reduce functions

work flow demo:

```
input is (already) split into M files
  Input1 -> Map -> a,1 b,1
  Input2 -> Map ->     b,1
  Input3 -> Map -> a,1     c,1
                    |   |   |
                    |   |   -> Reduce -> c,1
                    |   -----> Reduce -> b,2
                    ---------> Reduce -> a,2
```

Limitations (not very general-purpose):

* No interaction or state (other than via intermediate output).
* No iteration, no multi-stage pipelines.
* No real-time or streaming processing.

fault tolerance:

* worker crashed (coordinator notices worker no longer responds for a certain amount of time)
* coordinator re-run the whole tasks of the failed Map/Reduce worker
* re-run a Map twice is safe because map is functional deterministic (same input will always get same output)
