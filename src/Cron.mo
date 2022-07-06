import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Trie "mo:base/Trie";
import Iter "mo:base/Iter";
import List "mo:base/List";
import Bool "mo:base/Bool";
import Buffer "mo:base/Buffer";
import Array "mo:base/Array";
import Debug "mo:base/Debug";
import Hash "mo:base/Hash";
import HashMap "mo:base/HashMap";
import T "./Types";

actor Echo {
 type FunctionType = T.FunctionType;
  type Timestamp = T.Timestamp;
  type TaskList = T.TaskList; 
  type ScheduleMap = T.ScheduleMap;
  type Response = T.Response;
  type ScheduleInterval = T.ScheduleInterval;
  type Task = T.Task;
  
  let taskExample: [Task] = [
    {schedule = {minute = null; hour = ?2; dayOfMonth = ?3; month = ?4; dayOfWeek = ?5}; function = func() {#Executed}}
    ];
 

  var scheduleTable: ScheduleMap = HashMap.HashMap<Timestamp, TaskList>(1, func (x,y){x == y}, Hash.hash);
  let cron = T.Schedule(taskExample, scheduleTable);
  cron.init();
  system func heartbeat(): async () {
    cron.run();
  };
  // public func updateHashMap (index:Nat, value:Text) : async [(Nat, TaskList)] {
  //     var existingList : ?TaskList = scheduleTable.get(index);
  //     var resList = convertOptionalList(existingList);
  //     resList := List.push(value, resList);
  //     scheduleTable.put(index, resList);

  //     let historyArr = Iter.toArray(scheduleTable.entries());
  // };

  // private func convertOptionalList (list : ?TaskList) : TaskList {
  //   switch (list) {
  //     case null {
  //       let newList : TaskList = List.nil();
  //     };
  //     case (?val) {
  //       val
  //     };
  //   };
  // };

  public func getFromHashMap ()  : async Nat {
    scheduleTable.size()
  };
};