BeginPackage["TAGATAME`"];
Unprotect @@ Names["TAGATAME`*"];
ClearAll @@ Names["TAGATAME`*"];

(*variables*)
tagatameRequest::usage="tagatameRequest[path,body]";
sessionId::usage="sessionId";
ticket::usage="";
quests::usage="quests as assoc";
generateSSID::usage="generateSSID[deviceid,secretid] generate a new session, cutting the existing session";
getQuests::usage="getQuests[] show quests as assoc";
battleStart::usage="battleStart[questid_] start a battle and return battle results. (need battleEnd to start another battle)";
getTrophyToReward::usage="getTrophyToReward[]";
battleEnd::usage="battleEnd[mission_,missiont_,trophytime_] mission 1 for mission uncleared, 0 for mission cleared; progress trophies.";
battleRetire::usage="battleRetire[missiont] "
battleId::usage="";
battleResult::usage="";
towerStart::usage="towerStart[floor] ";
towerEnd::usage="towerEnd[floor,damage,turn,trophytime]";
fragments::usage="";
questRaid::usage="questRaid[quest] need ticket";
storyTrophyExec::usage="storyTrophyExec[story,times]";
multiBtlStart::usage="multiBtlStart[quest]";
multiBtlEnd::usage="multiBtlEnd[trophytime]";
multiBtlRetire::usage="multiBtlRetire[] "
getTime::usage="getTime[] get server time"

Begin["`Private`"]
ticket=1;
sessionId=0;
quests;
battleId;
battleResult;
multiToken;
assetVer="a08b1dfcec2e1ed606bea87534665759d5fe428b_gumi"

(*Request*)
tagatameRequestWithoutSSID[path_,body_]:=Block[{req,guid},
guid=StringDelete[CreateUUID[],"-"];
req=HTTPRequest[
<|"Scheme"->"https",
"Domain"->"alchemist.gu3.jp",
"Path"->path,
Method -> "POST", 
"Headers"->{
"x-app-ver" -> "8bacd5cf:wd",
"X-GUMI-TRANSACTION"-> guid,
"X-GUMI-REQUEST-ID"->guid,
"X-GUMI-CLIENT"->"gscc ver.0.1",
"X-Unity-Version"-> "5.3.6p1",
"x-asset-ver"->assetVer,
"Content-Type"->"application/json; charset=utf-8",
"Content-Length"->Length@ImportString[body,{"Binary","Byte"}]},
"Body"-> body,
VerifySecurityCertificates -> False
|>];
ImportString[URLRead[req,"Body"],"RawJSON"]];

tagatameRequest[path_,body_]:=Block[{req,guid},
guid=StringDelete[CreateUUID[],"-"];
req=HTTPRequest[
<|"Scheme"->"https",
"Domain"->"alchemist.gu3.jp",
"Path"->path,
Method -> "POST", 
"Headers"->{
"User-Agent"->"UnityPlayer/5.3.6p1 (http://unity3d.com)",
"Host"->"alchemist.gu3.jp",
"Accept"->"*/*",
"Accept-Encoding"->"identity",
"Content-Type"->"application/json; charset=utf-8",
"X-GUMI-CLIENT"->"gscc ver.0.1",
"X-GUMI-DEVICE-OS"->"windows",
"X-GUMI-REQUEST-ID"->guid,
"X-GUMI-STORE-PLATFORM"->"dmmgamesstore",
"X-GUMI-TRANSACTION"-> guid,
"X-Gumi-Game-Environment"->"production",
"X-Unity-Version"-> "5.3.6p1",
"x-app-ver" -> "8bacd5cf:wd",
"x-asset-ver"->assetVer,
"Content-Length"->Length@ImportString[body,{"Binary","Byte"}],
"Authorization"->"gauth "<>sessionId},
"Body"-> body,
VerifySecurityCertificates -> False
|>];
ImportString[URLRead[req,"Body"],"RawJSON"]
]

getTime[]:=QuantityMagnitude[DateObject[Now,"Second"]-DateObject[{2017,8,11,12,12,03},"Second"]]+1502424733-10

generateSSID[deviceid_:"a7b429b5-88a0-4a5b-9082-495f526dc8a0",secretkey_:"ef9ac5ff-0818-46e0-9837-e1b20fd47b5a"]:=
Block[{json,body},
ticket=ticket+1;
json=<|"ticket"->ticket,"access_token"->"",  "param"-><|"device_id"->deviceid,"secret_key"->secretkey|>|>;
body=ExportString[json,"RawJSON","Compact"->True] ;
json=tagatameRequestWithoutSSID["/gauth/accesstoken",body];
sessionId=json["body"]["access_token"];
tagatameRequest["/product","{\"ticket\":"<>ToString[ticket]<>"}"];
Return[sessionId];
];
getQuests[]:=Block[{body},
ticket++;
body="{\"ticket\":"<>ToString[ticket]<>",\"param\":{\"event\":1}}";
quests=tagatameRequest["/btl/com",body]["body"]["quests"]
];

battleStart[questid_]:=Block[{json,body,result},
ticket++;
json=<|"ticket"->ticket,"param"-><|"iname"->questid,"partyid"->0,"req_at"->getTime[],"btlparam"-><|"help"-><|"fuid"->""|>|>,"location"-><|"lat"->0,"lng"->0|>|>|>;
body=ExportString[json,"RawJSON","Compact"->True] ;
result=tagatameRequest["/btl/com/req",body];
battleId=result["body"]["btlid"];
battleResult=result["body"];
result
];

getTrophyToReward[]:=Block[
{result},
ticket++;
result=tagatameRequest["/login/param","{\"ticket\":"<>ToString[ticket]<>"}"];
result=result["body"]["trophyprogs"];
Select[result,!IntegerQ[#["rewarded_at"]]&]
];

battleEnd[mission_,missiont_,trophytime_]:=Block[
{json,number,trophyprogs,body,today},
ticket++;
number=Length[battleResult["btlinfo"]["drops"]];
today=StringJoin[IntegerString[#,10,2]&/@DateValue[{"Year","Month","Day"}]];
trophyprogs=getTrophyToReward[];
(*trophyprogs=Select[trophyprogs,StringContainsQ[#["iname"],StringTake[battleResult["btlinfo"]["qid"],{7,-1}]]&];*)
trophyprogs=<|"iname"->#["iname"],"pts"->#["pts"]+trophytime,"ymd"->#["ymd"]|>&/@trophyprogs;
json=<|"ticket"->ticket,"param"-><|"btlid"->battleId,"btlendparam"-><|"time"->0,"result"->"win","beats"->Table[1,number],"steals"-><|"items"->Table[0,number],"golds"->Table[0,number]|>,"missions"->Table[mission,missiont],"inputs"->{}|>,"trophyprogs"->trophyprogs|>|>;
body=ExportString[json,"RawJSON","Compact"->True];
tagatameRequest["/btl/com/end",body]
];

battleRetire[missiont_]:=Block[
{json,number,trophyprogs,body,today},
ticket++;
number=Length[battleResult["btlinfo"]["drops"]];
json=<|"ticket"->ticket,"param"-><|"btlid"->battleId,"btlendparam"-><|"time"->0,"result"->"retire","beats"->Table[0,number],"steals"-><|"items"->Table[0,number],"golds"->Table[0,number]|>,"missions"->Table[0,missiont],"inputs"->{}|>,"trophyprogs"->{}|>|>;
body=ExportString[json,"RawJSON","Compact"->True];
tagatameRequest["/btl/com/end",body]
];

towerStart[floor_]:=Block[
{json,body,result},
ticket++;
json=<|"ticket"->ticket,"param"-><|"qid"->"QE_TW_BABEL","fid"->"TW_BABEL_FLOOR_"<>IntegerString[floor,10,3],"fuid"->""|>|>;
body=ExportString[json,"RawJSON","Compact"->True];
result=tagatameRequest["/tower/btl/req",body];
battleId=result["body"]["btlid"];
battleResult=result["body"];
result
];

towerEnd[floor_,damage_,turn_,trophytime_]:=Block[
{json,pdeck,trophyprogs,body},
ticket++;
trophyprogs=getTrophyToReward[];
trophyprogs=<|"iname"->#["iname"],"pts"->#["pts"]+trophytime,"ymd"->#["ymd"]|>&/@trophyprogs;
pdeck={<|"iid"->100490627,"iname"->"UN_V2_SQ_PRINCESS","damage"->damage,"is_died"->0|>};
json=<|"ticket"->ticket,"param"-><|"btlid"->battleId,"btlendparam"-><|"pdeck"->pdeck,"status"->"win","turn"->turn,"round"->0,"floor"->floor|>,"trophyprogs"->trophyprogs|>|>;
body=ExportString[json,"RawJSON","Compact"->True];
tagatameRequest["/tower/btl/end",body]
];

questRaid[quest_]:=Block[{json,body},
ticket++;
json=<|"ticket"->ticket,"param"-><|"iname"->quest,"partyid"->0,"req_at"->getTime[],"ticket"->1|>|>;
body=ExportString[json,"RawJSON","Compact"->True];
tagatameRequest["/btl/com/raid2",body]
];

storyTrophyExec[story_,times_]:=Block[
{json,body,trophyprogs,today},
ticket++;
trophyprogs=getTrophyToReward[];
trophyprogs=<|"iname"->#["iname"],"pts"->#["pts"]+times,"ymd"->#["ymd"]|>&/@trophyprogs;
today=StringJoin[IntegerString[#,10,2]&/@DateValue[{"Year","Month","Day"}]];
trophyprogs=Append[trophyprogs,<|"iname"->"WINQUEST_"<>IntegerString[story,10,3],"pts"->{1},"ymd"->today|>];
json=<|"ticket"->ticket,"param"-><|"trophyprogs"->trophyprogs|>|>;
body=ExportString[json,"RawJSON","Compact"->True];
tagatameRequest["/trophy/exec",body]
];

makeRoom[quest_]:=Block[
{json,body},
ticket++;
json=<|"ticket"->ticket,"param"-><|"iname"->quest,"comment"->"","pwd"->"1","private"->0,"req_at"->getTime[],"limit"->0,"unitlv"->0,"clear"->0|>|>;
body=ExportString[json,"RawJSON","Compact"->True];
tagatameRequest["btl/room/make",body]
];

multiBtlStart[quest_]:=Block[{token,json,body,result},
token=makeRoom[quest]["body"]["token"];
multiToken=token;
ticket++;
json=<|"ticket"->ticket,"param"-><|"iname"->quest,"partyid"->2,"token"->token,"host"->"1","plid"->"1","seat"->"1","btlparam"-><|"help"-><|"fuid"->""|>|>,"location"-><|"lat"->0,"lng"->0|>|>|>;
body=ExportString[json,"RawJSON","Compact"->True] ;
result=tagatameRequest["/btl/multi/req",body];
battleId=result["body"]["btlid"];
battleResult=result["body"];
result
];

multiBtlEnd[trophytime_]:=Block[
{json,number,trophyprogs,body,today},
ticket++;
number=Length[battleResult["btlinfo"]["drops"]];
today=StringJoin[IntegerString[#,10,2]&/@DateValue[{"Year","Month","Day"}]];
trophyprogs=getTrophyToReward[];
trophyprogs=<|"iname"->#["iname"],"pts"->#["pts"]+trophytime,"ymd"->#["ymd"]|>&/@trophyprogs;
json=<|"ticket"->ticket,"param"-><|"btlid"->battleId,"btlendparam"-><|"time"->0,"result"->"win","beats"->Table[1,number],"steals"-><|"items"->Table[0,number],"golds"->Table[0,number]|>,"missions"->{},"inputs"->{},"token"->multiToken|>,"fuids"->{},"trophyprogs"->trophyprogs|>|>;
body=ExportString[json,"RawJSON","Compact"->True];
tagatameRequest["/btl/multi/end",body]
];

multiBtlRetire[]:=Block[
{json,number,body},
ticket++;
number=Length[battleResult["btlinfo"]["drops"]];
json=<|"ticket"->ticket,"param"-><|"btlid"->battleId,"btlendparam"-><|"time"->0,"result"->"retire","beats"->Table[0,number],"steals"-><|"items"->Table[0,number],"golds"->Table[0,number]|>,"missions"->{},"inputs"->{},"token"->multiToken|>,"fuids"->{},"trophyprogs"->{}|>|>;
body=ExportString[json,"RawJSON","Compact"->True];
tagatameRequest["/btl/multi/end",body]
];

End[]
EndPackage[]
