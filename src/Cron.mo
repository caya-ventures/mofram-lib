import Array "mo:base/Array";
import CronParser "./CronParser";
import DateTime "./DateTime";
import HashMap "mo:base/HashMap";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Time "mo:base/Time";

module {
    public type FunctionType = () -> ();
    public type Timestamp = Int;
    public type TaskList = List.List<Task>; 
    public type ScheduleMap = HashMap.HashMap<Timestamp, TaskList>;
    public type NextTask = { timestamp: Timestamp;task: Task; var status: Nat };
    public type Response = {
        #Executed;
        #NotExecuted;
    };

    public type CronExpression = Text;

    public type Task = {
        schedule: CronExpression;
        function: FunctionType;
    };

    public class Cron (task: Task) {
        var scheduleMap: ScheduleMap = HashMap.HashMap<Timestamp, TaskList>(1, func (x,y) {x == y}, Int.hash);
        public var cronObject = CronParser.parseCronExpression(task.schedule);

        public func construct() {
            updateHashMap(getNextDate(Time.now()), task);
        };

        public func updateHashMap (index:Timestamp, value:Task) :  () {
            var existingList : ?TaskList = scheduleMap.get(index);
            var resList = convertOptionalList(existingList);
            resList := List.push(value, resList);

            scheduleMap.put(index, resList);
        };

        private func convertOptionalList (list : ?TaskList) : TaskList {
            switch (list) {
                case null {
                    let newList : TaskList = List.nil();
                };
                    case (?val) {
                    val
                };
            };
        };

        // Iterate tasks in the storage and execute tasks' function.
        public func run() {
            for ((timestamp, taskList) in scheduleMap.entries()) {
                let time = Time.now();
                if(Time.now() >= timestamp) {
                    List.iterate(taskList, func(task: Task) {
                        task.function();
                        let timeSchedule = getNextDate(time + 1000000000);
                        updateHashMap(timeSchedule, task);
                    });

                    scheduleMap.delete(timestamp);
                }
            };
        };

        // Gets the next date starting from the given start date or now.
        public func getNextDate(time: Time.Time): Timestamp {
            let startDateElements : DateTime.DateTime = DateTime.timestampToDateTime(time); 
            var minYear : DateTime.Year = startDateElements.year;
            let startMonth = Array.find<Nat>(cronObject.months, func(x : Nat) { x >= startDateElements.month}); 

            var startIndexMonth : Nat = 0;
            switch (startMonth) {
                case (null) {
                    startIndexMonth := 0;
                    minYear += 1;
                };
                case (?value) {
                    var j = 0;
                    label monthLoop for (i in cronObject.months.vals()) {
                        if (i == value) {
                            break monthLoop;
                        };
                        j += 1;
                    };

                    startIndexMonth := j;
                };
            };

            // Try every month within the next 5 years to find a matching date inside a whole leap year.
            let maxIterations = cronObject.months.size() * 5;
            var isStartMonth : Bool = true;
            var year : Nat = 0;
            var month : Nat = 0;
            for (iterations in Iter.range(0, maxIterations)) {
                // Get the next year and month.
                year := Int.abs(minYear) + ((startIndexMonth + iterations) / cronObject.months.size());
                month := cronObject.months[(startIndexMonth + iterations) % cronObject.months.size()];
                isStartMonth := year == startDateElements.year and month == startDateElements.month;

                // Find the next day.
                let startDay = if (isStartMonth) {startDateElements.day} else {1};
                var day = findAllowedDayInMonth(year, month, Int.abs(startDay));
                var isStartDay = isStartMonth and day == startDateElements.day;
                let elements = {
                    hour = startDateElements.hour;
                    minute = startDateElements.min;
                    second = startDateElements.sec
                };

                switch (day) {
                    case null {};
                    // Find a valid time starting from the start date time.
                    case (?day) {
                        if (isStartDay == true) {
                            let nextTime = findAllowedTime(elements);
                            switch (nextTime) {
                                case null {};
                                case (?nextTime) {
                                    return Int.abs(DateTime.dateTimeToTimestamp({
                                        year = year;
                                        month = month;
                                        day = day;
                                        week_day = #Monday;
                                        hour = nextTime.hour;
                                        min = nextTime.minute;
                                        sec = nextTime.second;
                                    }));
                                };
                            };
                            // If no valid time has been found for the start date, try the next day.
                            isStartDay := false;
                            var newDay = findAllowedDayInMonth(year, month, day + 1);
                        };
                    };
                };

                // If the day is not the start day use the next day with the first allowed values for hours, minutes and seconds.
                switch (day) {
                    case null {};
                    case (?day) {
                        if (isStartDay == false) {
                            return Int.abs(DateTime.dateTimeToTimestamp({
                                year = year;
                                month = month;
                                day = day;
                                week_day = #Monday;
                                hour = cronObject.hours[0];
                                min = cronObject.minutes[0];
                                sec = cronObject.seconds[0];
                            }));
                        };
                    };
                };
            };

            return Int.abs(DateTime.dateTimeToTimestamp({
                year = 1970;
                month = 1;
                day = 1;
                week_day = #Thursday;
                hour = 0;
                min = 0;
                sec = 0;
            }));
        };

        public func testGetDayOfWeek (year : Nat, month : Nat, startDay : Nat) : async DateTime.DayOfWeek {
            let dateTimeObj = {
                year = year;
                month = month;
                day = startDay;
                week_day = #Wednesday;
                hour = 0;
                min = 0;
                sec = 0;
            };

            let tmpTimestamp = DateTime.dateTimeToTimestamp(dateTimeObj);
            let startWeekday = DateTime.timestampToDateTime(tmpTimestamp).week_day;
        };


        // Find the next day in the month starting from the startDay that matches either the day or the weekday constraint.
        private func findAllowedDayInMonth(year : Nat, month : Nat, startDay : Nat): ?Nat {
            let daysInMonth = getDaysInMonth(year, month);

            let daysRestricted : Bool = cronObject.days.size() != 31;
            let weekdaysRestricted : Bool = cronObject.weekdays.size() != 7;

            if (daysRestricted == false and weekdaysRestricted == false) {
                if (startDay > daysInMonth) {
                    return null;
                };

                return ?startDay;
            };

            var allowedDayByDays : ?Nat = null;
            if (daysRestricted) {
                allowedDayByDays := Array.find<Nat>(cronObject.days, func(x : Nat) { x >= startDay });
                let allowedDayByDays2 : Int = switch allowedDayByDays {
                    case null 0;
                    case (?int) int;
                };

                if (allowedDayByDays2 != 0 and allowedDayByDays2 > daysInMonth) {
                    allowedDayByDays := null;
                };
            };

            var allowedDayByWeekdays : ?Nat = null;
            if (weekdaysRestricted) {
                let dateTimeObj = {
                    year = year;
                    month = month;
                    day = startDay;
                    week_day = #Sunday;
                    hour = 0;
                    min = 0;
                    sec = 0;
                };

                let tmpTimestamp = DateTime.dateTimeToTimestamp(dateTimeObj);
                let startWeekdayHumanReadable = DateTime.timestampToDateTime(tmpTimestamp).week_day;
                let startWeekday = DateTime.weekDayFromHumanReadable(startWeekdayHumanReadable);

                let tmpNearestAllowedWeekday = Array.find<Nat>(cronObject.weekdays, func(x : Nat) { x >= startWeekday});

                var nearestAllowedWeekday : Nat = 0;
                switch (tmpNearestAllowedWeekday) {
                    case (null) {
                        nearestAllowedWeekday := cronObject.weekdays[0];
                    };
                    case (?value) {
                        nearestAllowedWeekday := value;
                    }
                };

                let daysBetweenWeekdays = getDaysBetweenWeekdays(startWeekday, nearestAllowedWeekday);
                allowedDayByWeekdays := ?(startDay + daysBetweenWeekdays);

                let allowedDayByWeekdays2 : Int = switch allowedDayByWeekdays {
                    case null 0;
                    case (?int) int;
                };

                if (allowedDayByWeekdays2 > daysInMonth or allowedDayByWeekdays2 < 1) {
                    allowedDayByWeekdays := null;
                }
            };

            switch ((allowedDayByDays,allowedDayByWeekdays)) {
                case (?val1, ?val2) {
                    return ?Nat.min(val1, val2)
                };
                case (_,_) {}
            };

            if (allowedDayByDays != null) {
                return allowedDayByDays
            };

            if (allowedDayByWeekdays != null) {
                return allowedDayByWeekdays
            };

            return null;
        };

        private func getDaysBetweenWeekdays(weekday1 : Nat, weekday2 : Nat): Nat {
            if (weekday1 <= weekday2) {
                return weekday2 - weekday1;
            };

            return 6 - weekday1 + weekday2 + 1;
        };

        private func findAllowedHour(startHour : DateTime.Hour): ?DateTime.Hour {
            Array.find<Nat>(cronObject.hours, func(x : Nat) { x >= startHour });
        };

        private func findAllowedMinute(startMinute  DateTime.Minute): ?DateTime.Minute {
            Array.find<Nat>(cronObject.minutes, func(x : Nat) { x >= startMinute });
        };

        private func findAllowedSecond(startSecond : DateTime.Second): ?DateTime.Second {
            Array.find<Nat>(cronObject.seconds, func(x : Nat) { x >= startSecond });
        };

        private func getDaysInMonth(year : Nat, month : Nat) : Nat {
            switch (month) {
                case (1) 31;
                case (2) {
                    if (DateTime.isLeapYear(year)) {
                        29;
                    } else {
                        28;
                    };
                }; 
                case (3) 31;
                case (4) 30;
                case (5) 31;
                case (6) 30;
                case (7) 31;
                case (8) 31;
                case (9) 30;
                case (10) 31;
                case (11) 30;
                case (12) 31;
            };
        };

        // Find the next time starting from the startTime that matches the hour, minute and second constraints.
        private func findAllowedTime(
            startTime: { hour : DateTime.Hour; minute : DateTime.Minute; second : DateTime.Second }
        ) : ?{ hour : DateTime.Hour; minute : DateTime.Minute; second : DateTime.Second } {
            // Try to find an allowed hour.
            var hour : ?DateTime.Hour = findAllowedHour(startTime.hour);

            switch (hour) {
                case null {};
                case (?hour) {
                    let iHour : DateTime.Hour = hour;
                    if (hour == startTime.hour) {
                        var minute : ?DateTime.Minute = findAllowedMinute(startTime.minute);

                        switch (minute) {
                            case null {};
                            case (?minute) {
                                if (minute == startTime.minute) {
                                    let second : ?DateTime.Second = findAllowedSecond(startTime.second);
                                    switch (second) {
                                        case null {                                            
                                            var minute : ?DateTime.Minute = findAllowedMinute(startTime.minute + 1);
                                            switch (minute) {
                                                case null {};
                                                case (?minute) {
                                                    return ?{
                                                        hour = iHour;
                                                        minute = minute;
                                                        second = cronObject.seconds[0];
                                                    };
                                                };
                                            };
                                        };
                                        case (?second) {
                                            return ?{ hour = iHour; minute = minute; second = second };
                                        }
                                    };
                                } else {
                                    return ?{
                                        hour = iHour;
                                        minute = minute;
                                        second = cronObject.seconds[0];
                                    }
                                }
                            }
                        };

                        var hour : ?DateTime.Hour = findAllowedHour(startTime.hour + 1);
                        switch (hour) {
                            case null {};
                            case (?hour) {
                                return ?{
                                    hour = hour;
                                    minute = cronObject.minutes[0];
                                    second = cronObject.seconds[0];
                                }
                            };
                        };
                    } else {
                        return ?{
                            hour = hour;
                            minute = cronObject.minutes[0];
                            second = cronObject.seconds[0];
                        }
                    }
                };
            };

            // No allowed time found.
            return null;
        };

        construct();
    }
}