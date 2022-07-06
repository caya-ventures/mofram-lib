import Buffer "mo:base/Buffer";
import Iter "mo:base/Iter";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Int "mo:base/Int";
import Nat32 "mo:base/Nat32";
import Char "mo:base/Char";
import List "mo:base/List";
import Debug "mo:base/Debug";
import Prim "mo:prim";

module {
  public type ParsedExpression = {
    seconds : Buffer.Buffer<Int>;
    minutes : Buffer.Buffer<Int>;
    hours : Buffer.Buffer<Int>;
    days : Buffer.Buffer<Int>;
    months : Buffer.Buffer<Int>;
    weekdays : Buffer.Buffer<Int>;
  };

  public type ParsedExpressionArray = {
    seconds : [Int];
    minutes : [Int];
    hours : [Int];
    days : [Int];
    months : [Int];
    weekdays : [Int];
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

  private func timeShortcut(cronExpression: Text): Text {
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
  public func parseCronExpression(cronExpression: Text): ParsedExpressionArray {
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
    let monthsCron = Buffer.Buffer<Int>(1);
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

  public func parseElement(element: Text, limit: Limit): Buffer.Buffer<Int> {
    var result = Buffer.Buffer<Int>(1);
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
        case (?rangeElement) parsedStart := parseSingleElementNat(rangeElement, limit);
      };
      rangeElement := List.get(rangeSegmentsList, 1);
      var parsedEnd: Nat = 0;
      switch (rangeElement) {
        case null {};
        case (?rangeElement) parsedEnd := parseSingleElementNat(rangeElement, limit);
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
        case (?divider) parsedStep := parseSingleElementNat(divider, limit);
      };
      while(parsedStart <= limit.max) {
        result.add(parsedStart);
        parsedStart := parsedStart + parsedStep;
      };

      return result;
    };
    result.add(parseSingleElement(element, limit));

    return result;
  };

  private func parseSingleElement(singleElement: Text, limit: Limit): Int {
    var parsedElement = textToInt(singleElement);

    if (parsedElement < limit.min or parsedElement > limit.max) {
      Debug.trap("Element "#Int.toText(parsedElement)#" is out of range");
    };

    return parsedElement;
  };

  private func parseSingleElementNat(singleElement: Text, limit: Limit): Nat {
    var parsedElement = textToNat(singleElement);

    if (parsedElement < limit.min or parsedElement > limit.max) {
      Debug.trap("Failed to parse "#singleElement#": "#singleElement#" is outside of constraint range of "#Int.toText(limit.min)#" - "#Int.toText(limit.min)#"");
    };
    return parsedElement;
  };

  private func textToInt( txt : Text) : Int {
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
  }

};