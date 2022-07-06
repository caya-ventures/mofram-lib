import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import List "mo:base/List";
import Debug "mo:base/Debug";

module {

  public type FunctionType = () -> Response;
  public type Timestamp = Nat;
  public type TaskList = List.List<Task>; 
  public type ScheduleMap = HashMap.HashMap<Timestamp, TaskList>;

  public type Response = {
    #Executed;
    #Error;
    #NotExecuted;
  };

  public type ScheduleInterval = {
    minute: ?Nat;
    hour: ?Nat;
    dayOfMonth: ?Nat;
    month: ?Nat;
    dayOfWeek: ?Nat;
  };
  
  public type Task = {
    schedule: ScheduleInterval;
    function: FunctionType;
  };
  public class Schedule (tasks: [Task], scheduleMap: ScheduleMap) {
    
    var nanoseconds: Nat = 1000000000;
    var minute: Nat = 60 * nanoseconds;
    var hour: Nat = 3600 * nanoseconds;
    var day: Nat = 86400 * nanoseconds;

    public func init() {
      var textList : TaskList = List.nil(); 
      for(task in tasks.vals()){
        //TODO: schedule generator
        let timestamp: Timestamp = 0;
         textList := List.push(task, textList);
         scheduleMap.put(timestamp, textList);
      };
    };
    public func run() {
      Debug.print("test");
        //look for nearest timestamp in hashMap and execute functions from it
    }
  }
}