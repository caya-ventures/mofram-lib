# Mofram library
Contains a set of libs to work with Cron, Date and Time and Array.

## Cron

A cron scheduler and cron parser for motoko. Uses a well-known linux cron format with the additional field for seconds.
Range of values and step values are supported.

```
* * * * * *
│ │ │ │ │ └───────────── weekday (0 - 7)
| | | | └───────────── month (1 - 12)
| | | └───────────── day of month (1 - 31) 
| | └───────────── hour (0 - 23)
| └───────────── minute (0 - 59)
└───────────── second (0 - 59)
```

Installation:
Via vessel, for more details see - https://github.com/dfinity/vessel


Usage sample:

```motoko
import Cron "mo:mofram-lib/Cron";
import Debug "mo:base/Debug";
import Time "mo:base/Time";

actor {
    private func testFunc10sec(): () {
        Debug.print("Every 10 second: "#debug_show Time.now());
    };

    private func testFunc1min(): () {
        Debug.print("Every minute: "#debug_show Time.now());
    };

    private func testFunc1hour(): () {
        Debug.print("Every hour: "#debug_show Time.now());
    };

    let cron10sec = Cron.Cron({schedule = "*/10 * * * * *"; function = testFunc10sec});
    let cron1min = Cron.Cron({schedule = "* * * * *"; function = testFunc1min});
    let cron1hour = Cron.Cron({schedule = "0 * * * *"; function = testFunc1hour});

    system func heartbeat(): async () {
        cron10sec.run();
        cron1min.run();
        cron1hour.run();
    };
};
```

## Array
An array library that provides useful functions to work with motoko arrays. All functions are covered with unit tests using https://github.com/kritzcreek/motoko-matchers

## DateTime
Library with useful methods for date and time.

## License

mofram-library is distributed under the terms of the Apache License (Version 2.0).

See LICENSE for details.