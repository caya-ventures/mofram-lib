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

module {

  public type FunctionType = () -> Response;
  public type Timestamp = Int;
  public type TaskList = List.List<Task>; 
  public type ScheduleMap = HashMap.HashMap<Timestamp, TaskList>;

  public type Response = {
    #Executed;
    #Error;
    #NotExecuted;
  };

  public type CronExpression = Text;
  
  public type Task = {
    schedule: CronExpression;
    function: FunctionType;
  };

  public type ParsedExpression = {
    seconds : [Nat];
    minutes : [Nat];
    hours : [Nat];
    days : [Nat];
    months : [Nat];
    weekdays : [Nat];
  };

  public type Limit = {
    min: Nat;
    max: Nat;
  };

  public let secondLimit: Limit = {
    min = 0;
    max = 59;
  };

  let minuteLimit: Limit = {
    min = 0;
    max = 59;
  };

  let hourLimit: Limit = {
    min = 0 ;
    max = 23 ;
  };

  let dayLimit: Limit = {
    min = 1 ;
    max = 31 ;
  };

  let monthLimit: Limit = {
    min = 1 ;
    max = 12 ;
  };

  let weekdayLimit: Limit = {
    min = 0 ;
    max = 6 ;
  };

  private func timeShortcut(cronExpression: CronExpression): CronExpression {
    switch (Text.map(cronExpression , Prim.charToLower)) {
      case ("@yearly") "0 0 1 1 *";
      case ("@annually") "0 0 1 1 *";
      case ("@monthly") "0 0 1 * *";
      case ("@weekly") "0 0 * * 0";
      case ("@daily") "0 0 * * *";
      case ("@hourly") "0 * * * *";
      case ("@minutely") "* * * * *";
      case (_) cronExpression;
    };
  };

  public func parseCronExpression(cronExpression: CronExpression): ParsedExpression {
    var expression: Text = timeShortcut(cronExpression);
    let elements = Iter.toList(Text.split(expression, #text " "));

    if (List.size(elements) < 5 or List.size(elements) > 6) {
      Debug.trap("Invalid cron expression: expected 5 or 6 elements.");
    };
    let rawSeconds = if (List.size(elements) == 6) { getFromTextList(elements,0) } else { "0" };
    let rawMinutes = if (List.size(elements) == 6) { getFromTextList(elements,1) } else { getFromTextList(elements,0) };
    let rawHours = if (List.size(elements) == 6) { getFromTextList(elements,2) } else { getFromTextList(elements,1) };
    let rawDays = if (List.size(elements) == 6) { getFromTextList(elements,3) } else { getFromTextList(elements,2) };
    let rawMonths = if (List.size(elements) == 6) { getFromTextList(elements,4) } else { getFromTextList(elements,3) };
    let rawWeekdays = if (List.size(elements) == 6) { getFromTextList(elements,5) } else { getFromTextList(elements,4) };
    let monthsCron = Buffer.Buffer<Nat>(1);
    for(i in parseElement(rawMonths,monthLimit).vals()) {
      monthsCron.add(i-1);
    };
    {
      seconds = parseElement(rawSeconds, secondLimit).toArray();
      minutes = parseElement(rawMinutes, minuteLimit).toArray();
      hours = parseElement(rawHours, hourLimit).toArray();
      days = parseElement(rawDays, dayLimit).toArray();
      months = monthsCron.toArray();
      weekdays = parseElement(rawWeekdays, weekdayLimit).toArray();
    };    
  };

  public func parseElement(element: Text, limit: Limit): Buffer.Buffer<Nat> {
    var result = Buffer.Buffer<Nat>(1);
    if (element == "*") {
      for (i in Iter.range(limit.min, limit.max)) {
        result.add(i);
      };

      return result;
    };
    let listElementsList = Iter.toList(Text.split(element, #text ","));
    if (Nat.greater(List.size(listElementsList),1)) {
       List.iterate<Text>(listElementsList, func(listElement) {
        for(x in  parseElement(listElement, limit).vals()) {
          result.add(x);
        };
      });

      return result;
    };

    let rangeSegmentsList = Iter.toList(Text.split(element, #text "-"));
    if (Nat.greater(List.size(rangeSegmentsList),1)) {
      var rangeElement = List.get(rangeSegmentsList, 0);
      var parsedStart: Nat = 0;
      switch (rangeElement) {
        case null {};
        case (?rangeElement) parsedStart := validateSingleElement(rangeElement, limit);
      };
      rangeElement := List.get(rangeSegmentsList, 1);
      var parsedEnd: Nat = 0;
      switch (rangeElement) {
        case null {};
        case (?rangeElement) parsedEnd := validateSingleElement(rangeElement, limit);
      };
      for (i in Iter.range(parsedStart, parsedEnd)) {
        result.add(i);
      };

      return result;
    };

    let stepSegmentsList = Iter.toList(Text.split(element, #text "/"));
    if (Nat.greater(List.size(stepSegmentsList),1)) {
      var stepElement = List.get(stepSegmentsList, 0);
      var parsedStart: Nat = 0;
      var parsedEnd: Nat = 0;
      switch (stepElement) {
        case null { return result};
        case (?stepElement) {
          if(stepElement == "*") {
            parsedStart := limit.min;
            };
          };
      };
      var divider = List.get(stepSegmentsList, 1);
      var parsedStep = 1;
      switch (divider) {
        case null {return result};
        case (?divider) parsedStep := validateSingleElement(divider, limit);
      };
      while(parsedStart <= limit.max) {
        result.add(parsedStart);
        parsedStart := parsedStart + parsedStep;
      };

      return result;
    };
    result.add(validateSingleElement(element, limit));

    return result;
  };

  private func validateSingleElement(singleElement: Text, limit: Limit): Nat {
    var parsedElement = textToNat(singleElement);

    if (parsedElement < limit.min or parsedElement > limit.max) {
      Debug.trap("Failed to parse "#singleElement#": "#singleElement#" is outside of constraint range of "#Nat.toText(limit.min)#" - "#Nat.toText(limit.min)#".");
    };

    return parsedElement;
  };

  private func textToNat( txt : Text) : Nat {
    assert(txt.size() > 0);
    let chars = txt.chars();

    var num : Nat = 0;
    for (v in chars){
        let charToNum = Nat32.toNat(Char.toNat32(v)-48);
        assert(charToNum >= 0 and charToNum <= 9);
        num := num * 10 +  charToNum;          
    };

    num;
  };
  
  private func getFromTextList(list: List.List<Text>, elem: Nat): Text {
    var listElement = List.get(list, elem);
    switch (listElement) {
      case null "";
      case (?listElement) listElement;
    };
  };


  public class Schedule (task: Task, scheduleMap: ScheduleMap) {
    
    var cronObject := CP.parseCronExpression(task.schedule);
    init();
    

    public func init() {
      var textList : TaskList = List.nil(); 
      textList := List.push(task, textList);
      scheduleMap.put(getNextDate(), textList);
    };
    public func run() {
      Debug.print("test");
        //look for nearest timestamp in hashMap and execute functions from it
    };
     /** Gets the next date starting from the given start date or now. */
    public func getNextDate(): Timestamp {
      let startDateElements : DateTime.DateTime = DateTime.timestampToDateTime(Time.now()); 
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
                    month = month + 1;
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
                month = month + 1;
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
      var allowedDayByWeekdays : ?Nat = ?0;
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
          case (0) 31;
          case (1) {
              if (isLeapYear(year)) {
                  29;
              } else {
                  28;
              };
          }; 
          case (2) 31;
          case (3) 30;
          case (4) 31;
          case (5) 30;
          case (6) 31;
          case (7) 31;
          case (8) 30;
          case (9) 31;
          case (10) 30;
          case (11) 31;
        };
      };

    public type HMS = {
      hour: Nat;
      minute: Nat;
      second: Nat;
    };

    public func testfindAllowedTime(startTime: {hour:DateTime.Hour;minute:DateTime.Minute;second:DateTime.Second}): async ?{hour:DateTime.Hour;minute:DateTime.Minute;second:DateTime.Second}  {
      findAllowedTime(startTime);
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
  }
}