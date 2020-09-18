-module(records).
-compile(export_all).
-include("rec.hrl").

-record(robot, {name, type=industrial, hobbies, details=[]}).
-record(user, {id, name, group, age}).

first_robot() -> 
	#robot{name="Mechatron", type=handmade, details=["moved by small made inside"]}.

car_factory(RobotName) -> 
	#robot{name=RobotName, hobbies="building cars"}.

admin_panel(#user{name=Name, group=admin}) ->
	Name ++ " is allowed";
admin_panel(#user{name=Name}) -> 
	Name ++ " not allowed".

adult_section(U=#user{}) when U#user.age >= 18 -> 
	allowed;
adult_section(_) -> 
	not_allowed.

repaireman(R) -> 
	Details = R#robot.details,
	NewRobot = R#robot{details=["Just repaired this robot"|Details]},
	{repaired, NewRobot}.

get_included() -> #included{name="Included record here"}.