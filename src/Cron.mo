import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";
import Buffer "mo:base/Buffer";
import Iter "mo:base/Iter";
import Text "mo:base/Text";
import Int "mo:base/Int";
import Nat32 "mo:base/Nat32";
import Char "mo:base/Char";
import List "mo:base/List";
import Prim "mo:prim";
import Array "mo:base/Array";
import Time "mo:base/Time";
import DateTime "./DateTime";
import CronParser "./CronParser";

module {

  public type FunctionType = () -> ();
  public type Timestamp = Int;
  public type TaskList = List.List<Task>; 
  public type ScheduleMap = HashMap.HashMap<Timestamp, TaskList>;
  public type NextTask = {timestamp: Timestamp;task: Task; var status: Nat};
  public type Response = {
    #Executed;
    #NotExecuted;
  };

  public type CronExpression = Text;
  
  public type Task = {
    schedule: CronExpression;
    function: FunctionType;
    //var status: Nat;
  };
  public class Cron (task: Task) {
    //public var nextTask: NextTask = {timestamp = 0;task = {schedule = ""; function = func (): () {}}; var status = 0};
    var scheduleMap: ScheduleMap = HashMap.HashMap<Timestamp, TaskList>(1, func (x,y){x == y}, Int.hash);
    public var cronObject = CronParser.parseCronExpression(task.schedule);

    public func construct() {
      // var textList : TaskList = List.nil(); 
      // textList := List.push(task, textList);
      // scheduleMap.put(getNextDate(), textList);
      updateHashMap(getNextDate(Time.now()), task);
      //nextTask := {timestamp = getNextDate(Time.now());task = task; var status = 0};
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

    public func run() {
        for((timestamp, taskList) in scheduleMap.entries()) {
          let time = Time.now();
          Debug.print("-------------------");
          var i: Nat = 0;
          if(Time.now() >= timestamp) {
            List.iterate(taskList, func(task1: Task) {
                task1.function();
                Debug.print("Func complete");
                let timeSchedule = getNextDate(time+1000000000);
                Debug.print(debug_show timeSchedule);
                updateHashMap(timeSchedule, task1);
               
                Debug.print("Hashmap complete");
                i := i+1;
            });
            scheduleMap.delete(timestamp);
          }
        };
        //look for nearest timestamp in hashMap and execute functions from it
    };

    // public func run(): async () {
    //       Debug.print("-------------------");
    //       let time = Time.now();
    //       var i: Nat = 0;
    //       if(Time.now() >= nextTask.timestamp and nextTask.status == 0) {
    //             nextTask.status := 1;
    //             let timeSchedule = getNextDate(time+1000000000);
    //             nextTask.task.function();
    //             nextTask := {timestamp = getNextDate(Time.now());task = task; var status = 0};
    //         };
        
    //     //look for nearest timestamp in hashMap and execute functions from it
    // };
     /** Gets the next date starting from the given start date or now. */
    public func getNextDate(time: Time.Time): Timestamp {
      let startDateElements : DateTime.DateTime = DateTime.timestampToDateTime(time); 
      var minYear : DateTime.Year = startDateElements.year;
      let startMonth = Array.find<Nat>(cronObject.months, func(x : Nat) { x >= startDateElements.month}); //month starts with 1. month = 7 
      
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
      // We try every month within the next 5 years to make sure that we tried to
      // find a matching date insidde a whole leap year.
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
        var day = findAllowedDayInMonth(year,month,Int.abs(startDay));
        var isStartDay = isStartMonth and day == startDateElements.day;
        let elements = {
          hour = startDateElements.hour;
          minute = startDateElements.min;
          second = startDateElements.sec
        };
      
        // If we found a day and it is the start day, try to find a valid time beginning from the start date time.
        switch (day) {
          case null {};
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

        // If we found a next day and it is not the start day, just use the next day with the first allowed values
        // for hours, minutes and seconds.
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

    public func testGetDayOfWeek (year : Nat, month: Nat, startDay:Nat) : async DateTime.DayOfWeek {
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

    /**
      * Find the next or previous day in the given month, starting from the given startDay
      * that matches either the day or the weekday constraint. startDay itself might also be allowed.
      */
    /**
      * Find the next or previous day in the given month, starting from the given startDay
      * that matches either the day or the weekday constraint. startDay itself might also be allowed.
      */
    private func findAllowedDayInMonth(year: Nat, month: Nat, startDay: Nat): ?Nat {
      // If only days are restricted: allow day based on day constraint only.
      // If only weekdays are restricted: allow day based on weekday constraint only.
      // If both are restricted: allow day based on both day and weekday constraint. pick day that is closer to startDay.
      // If none are restricted: return the day closest to startDay (respecting dir) that is allowed (or startDay itself).
      let daysInMonth = getDaysInMonth(year, month);
     
      let daysRestricted : Bool = cronObject.days.size() != 31;
      let weekdaysRestricted : Bool = cronObject.weekdays.size() != 7;

      if (daysRestricted == false and weekdaysRestricted == false) {
        if (startDay > daysInMonth) {
          return null;
        };

        return ?startDay;
      };

      // Try to find a day based on the days constraint.
      var allowedDayByDays : ?Nat = null;
      if (daysRestricted) {
        allowedDayByDays := Array.find<Nat>(cronObject.days, func(x: Nat) { x >= startDay });
        let allowedDayByDays2 : Int = switch allowedDayByDays {
          case null 0;
          case (?int) int;
        };

        // Make sure the day does not exceed the amount of days in month.
        if (allowedDayByDays2 != 0 and allowedDayByDays2 > daysInMonth) {
          allowedDayByDays := null;
        };
      };

      // Try to find a day based on the weekday constraint.
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

      private func getDaysBetweenWeekdays(weekday1: Nat, weekday2: Nat): Nat {
          if (weekday1 <= weekday2) {
              return weekday2 - weekday1;
          };

          return 6 - weekday1 + weekday2 + 1;
      };

    private func findAllowedHour(startHour : DateTime.Hour): ?DateTime.Hour {
      Array.find<Nat>(cronObject.hours, func(x: Nat) { x >= startHour });
    };

    private func findAllowedMinute(startMinute: DateTime.Minute): ?DateTime.Minute {
        Array.find<Nat>(cronObject.minutes, func(x: Nat) { x >= startMinute });
    };

    private func findAllowedSecond(startSecond: DateTime.Second): ?DateTime.Second {
        Array.find<Nat>(cronObject.seconds, func(x: Nat) { x >= startSecond });
    };

      private func isLeapYear(year : Nat) : Bool {
          Int.equal(Int.rem(year, 400), 0) or (Int.equal(Int.rem(year, 4), 0) and Int.notEqual(Int.rem(year, 100), 0));
      };

      private func getDaysInMonth(year: Nat, month: Nat) : Nat {
          switch (month) {
          case (1) 31;
          case (2) {
              if (isLeapYear(year)) {
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

    /**
      * Find the next or previous time, starting from the given start time that matches the hour, minute
      * and second constraints. startTime itself might also be allowed.
      */
    private func findAllowedTime(startTime: {hour:DateTime.Hour;minute:DateTime.Minute;second:DateTime.Second}): ?{hour:DateTime.Hour;minute:DateTime.Minute;second:DateTime.Second} {
      // Try to find an allowed hour.
      var hour : ?DateTime.Hour = findAllowedHour(startTime.hour);

      switch (hour) {
          case null {};
          case (?hour) {
            let iHour : DateTime.Hour = hour;
              if (hour == startTime.hour) {
                  // We found an hour that is the start hour. Try to find an allowed minute.
                  var minute : ?DateTime.Minute = findAllowedMinute(startTime.minute);
                  
                  switch (minute) {
                      case null {};
                      case (?minute) {
                          if (minute == startTime.minute) {
                              // We found a minute that is the start minute. Try to find an allowed second.
                              let second : ?DateTime.Second = findAllowedSecond(startTime.second);
                              switch (second) {
                                  case null {
                                      // We did not find a valid second within the start minute. Try to find another minute.
                                      var minute : ?DateTime.Minute = findAllowedMinute(startTime.minute + 1);
                                      switch (minute) {
                                          case null {};
                                          case (?minute) {
                                              // We found a minute which is not the start minute. Return that minute together with the hour and the first / last allowed second.
                                              return ?{
                                                  hour = iHour;
                                                  minute = minute;
                                                  second = cronObject.seconds[0];
                                              };
                                          };
                                      };
                                  };
                                  case (?second) {
                                      // We found a second within the start hour and minute.
                                      return ?{ hour = iHour; minute = minute; second = second };
                                  }
                              };
                          } else {
                              // We found a minute which is not the start minute. Return that minute together with the hour and the first / last allowed second.
                              return ?{
                                  hour = iHour;
                                  minute = minute;
                                  second = cronObject.seconds[0];
                              }
                          }
                      }
                  };

                  // We did not find an allowed minute / second combination inside the start hour. Try to find the next / previous allowed hour.
                  var hour : ?DateTime.Hour = findAllowedHour(startTime.hour + 1);
                  switch (hour) {
                      case null {};
                      case (?hour) {
                          // We found an allowed hour which is not the start hour. Return that hour together with the first / last allowed minutes / seconds.
                          return ?{
                              hour = hour;
                              minute = cronObject.minutes[0];
                              second = cronObject.seconds[0];
                          }
                      };
                  };
              } else {
                  // We found an allowed hour which is not the start hour. Return that hour together with the first / last allowed minutes / seconds.
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