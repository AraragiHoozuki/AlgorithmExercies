(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: Tagatame *)
(* :Context: Tagatame` *)
(* :Author: Rholin *)
(* :Date: 2018-01-22 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2018 Rholin *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["TAGATAME`"];
Unprotect @@ Names["TAGATAME`*"];
ClearAll @@ Names["TAGATAME`*"];

(*variables*)
registerAccount::usage="registerAccount[]";
tagatameRequestWithoutSSID::usage="tagatameRequestWithoutSSID[path_,body_]";
tagatameRequest::usage="tagatameRequest[account_,path_,body_]";
accountLogin::usage="accountLogin[deviceid,secretid] generate a new session, cutting the existing session";
battleStart::usage="battleStart[account_,questid_] start a battle and return battle results. (need battleEnd to start another battle)";
getTrophyToReward::usage="getTrophyToReward[account_]";
battleEnd::usage="battleEnd[account_,mission_,missiont_,trophytime_] mission 1 for mission uncleared, 0 for mission cleared; progress trophies.";
battleRetire::usage="battleRetire[account_,missiont]";
getTime::usage="getTime[] get server time";
makeRoom::usage="makeRoom[account_,quest_] make room";
joinRoom::usage="joinRoom[account_,roomid_]";
multiBtlStart::usage="multiBtlStart[account_,quest_,host_:1,plid_:1,seat_:1]";
multiBtlEnd::usage="multiBtlEnd[account_,trophytime_]";
multiBtlRetire::usage="multiBtlRetire[] ";
getQuests::usage="getQuests[account_] ";
shopUpdate::usage="shopUpdate[account_, shop_,cost_]";
buy::usage="buy[account_, shop_,id_,num_:1]";
trophyExec::usage="trophyExec[account_,iname_,pts_,day_]";

Begin["`Private`"]
iHost="alchemist.gu3.jp";
assetVer="039f46026e7f82360b7997e0a0e3b4b9576e0891_gumi";

(*Request*)
tagatameRequestWithoutSSID[path_,body_]:=Block[{req,guid},
  guid=StringDelete[CreateUUID[],"-"];
  req=HTTPRequest[
    <|"Scheme"->"https",
      "Domain"->iHost,
      "Path"->path,
      Method -> "POST",
      "Headers"->{
        "x-app-ver" -> "425b5e28:wd",
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
  ImportString[URLRead[req,"Body"],"RawJSON"]
];

tagatameRequest[account_,path_,body_]:=Block[
  {req,guid},
  guid=StringDelete[CreateUUID[],"-"];
  req=HTTPRequest[
    <|"Scheme"->"https",
      "Domain"->iHost,
      "Path"->path,
      Method -> "POST",
      "Headers"->{
        "User-Agent"->"UnityPlayer/5.3.6p1 (http://unity3d.com)",
        "Host"->iHost,
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
        "x-app-ver" -> "425b5e28:wd",
        "x-asset-ver"->assetVer,
        "Content-Length"->Length@ImportString[body,{"Binary","Byte"}],
        "Authorization"->"gauth "<>account["ssid"]},
      "Body"-> body,
      VerifySecurityCertificates -> False
    |>];
  ImportString[URLRead[req,"Body"],"RawJSON"]
];

getTime[]:=QuantityMagnitude[DateObject[Now,"Second"]-DateObject[{2017,8,11,12,12,03},"Second"]]+1502424733-10

accountLogin[deviceid_:"a7b429b5-88a0-4a5b-9082-495f526dc8a0",secretkey_:"ef9ac5ff-0818-46e0-9837-e1b20fd47b5a"]:= Block[
  {account=<||>,json,body},
  account["ticket"]=0;
  account["battle_result"]=Null;
  account["room"]=Null;
  account["ticket"]++;
  json=<|"ticket"->account["ticket"],"access_token"->"",  "param"-><|"device_id"->deviceid,"secret_key"->secretkey|>|>;
  body=ExportString[json,"RawJSON","Compact"->True] ;
  json=tagatameRequestWithoutSSID["/gauth/accesstoken",body];
  account["ssid"]=json["body"]["access_token"];
  tagatameRequest[account,"/product","{\"ticket\":"<>ToString[account["ticket"]]<>"}"];
  Return[account];
];

battleStart[account_,questid_]:=Block[
  {json,body,result},
  If[account["battle_result"]!=Null,Echo["There is unresolved battle","Error"];Return[]];
  account["ticket"]++;
  json=<|"ticket"->account["ticket"],"param"-><|"iname"->questid,"partyid"->0,"req_at"->getTime[],"btlparam"-><|"help"-><|"fuid"->""|>|>,"location"-><|"lat"->0,"lng"->0|>|>|>;
  body=ExportString[json,"RawJSON","Compact"->True] ;
  result=tagatameRequest[account,"/btl/com/req",body];
  account["battle_result"]=result
];
SetAttributes[battleStart, HoldFirst];

getTrophyToReward[account_]:=Block[
  {result},
  account["ticket"]++;
  result=tagatameRequest[account,"/login/param","{\"ticket\":"<>ToString[account["ticket"]]<>"}"];
  result=result["body"]["trophyprogs"];
  Select[result,!IntegerQ[#["rewarded_at"]]&]
];
SetAttributes[getTrophyToReward, HoldFirst];

battleEnd[account_,mission_,missiont_,trophytime_]:=Block[
  {json,number,trophyprogs,body,today},
  If[account["battle_result"]==Null,Echo["Battle not found.","Error"];Return[]];
  account["ticket"]++;
  number=Length[account["battle_result"]["body"]["btlinfo"]["drops"]];
  today=StringJoin[IntegerString[#,10,2]&/@DateValue[{"Year","Month","Day"}]];
  trophyprogs=getTrophyToReward[account];
  trophyprogs=<|"iname"->#["iname"],"pts"->#["pts"]+trophytime,"ymd"->#["ymd"]|>&/@trophyprogs;
  json=<|"ticket"->account["ticket"],"param"-><|"btlid"->account["battle_result"]["body"]["btlid"],"btlendparam"-><|"time"->0,"result"->"win","beats"->Table[1,number],"steals"-><|"items"->Table[0,number],"golds"->Table[0,number]|>,"missions"->Table[mission,missiont],"inputs"->{}|>,"trophyprogs"->trophyprogs|>|>;
  body=ExportString[json,"RawJSON","Compact"->True];
  account["battle_result"]=Null;
  tagatameRequest[account,"/btl/com/end",body]
];
SetAttributes[battleEnd, HoldFirst];

battleRetire[account_,missiont_]:=Block[
  {json,number,trophyprogs,body,today},
  If[account["battle_result"]==Null,Echo["Battle not found.","Error"];Return[]];
  account["ticket"]++;
  number=Length[account["battle_result"]["body"]["btlinfo"]["drops"]];
  json=<|"ticket"->account["ticket"],"param"-><|"btlid"->account["battle_result"]["body"]["btlid"],"btlendparam"-><|"time"->0,"result"->"retire","beats"->Table[0,number],"steals"-><|"items"->Table[0,number],"golds"->Table[0,number]|>,"missions"->Table[0,missiont],"inputs"->{}|>,"trophyprogs"->{}|>|>;
  body=ExportString[json,"RawJSON","Compact"->True];
  account["battle_result"]=Null;
  tagatameRequest[account,"/btl/com/end",body]
];
SetAttributes[battleRetire, HoldFirst];

makeRoom[account_,quest_]:=Block[
  {json,body},
  account["ticket"]++;
  json=<|"ticket"->account["ticket"],"param"-><|"iname"->quest,"comment"->"","pwd"->"1","private"->0,"req_at"->getTime[],"limit"->0,"unitlv"->0,"clear"->0|>|>;
  body=ExportString[json,"RawJSON","Compact"->True];
  account["room"]=tagatameRequest[account,"btl/room/make",body]
];
SetAttributes[makeRoom, HoldFirst];

joinRoom[account_,roomid_]:=Block[
  {json,body},
  account["ticket"]++;
  json=<|"ticket" -> account["ticket"], "param" -> <|"roomid" -> roomid, "pwd" -> "1"|>|>;
  body=ExportString[json,"RawJSON","Compact"->True];
  account["room"]=tagatameRequest[account,"btl/room/join",body]
];
SetAttributes[joinRoom, HoldFirst];

multiBtlStart[account_,quest_,host_:1,plid_:1,seat_:1]:=Block[
  {token,json,body,result},
  If[account["battle_result"]!=Null,Echo["There is unresolved battle","Error"];Return[]];
  If[account["room"]!=Null,Echo["Room not found","Error"];Return[]];
  account["ticket"]++;
  token=account["room"]["body"]["token"];
  json=<|"ticket"->account["ticket"],"param"-><|"iname"->quest,"partyid"->1,"token"->token,"host"->"1","plid"->"1","seat"->"1","btlparam"-><|"help"-><|"fuid"->""|>|>,"location"-><|"lat"->0,"lng"->0|>|>|>;
  body=ExportString[json,"RawJSON","Compact"->True] ;
  result=tagatameRequest[account,"/btl/multi/req",body];
  account["battle_result"]=result
];
SetAttributes[multiBtlStart, HoldFirst];

multiBtlEnd[account_,trophytime_]:=Block[
  {token,json,number,trophyprogs,body,today},
  If[account["battle_result"]==Null,Echo["Battle not found.","Error"];Return[]];
  account["ticket"]++;
  token=account["room"]["body"]["token"];
  number=Length[account["battle_result"]["body"]["btlinfo"]["drops"]];
  today=StringJoin[IntegerString[#,10,2]&/@DateValue[{"Year","Month","Day"}]];
  trophyprogs=getTrophyToReward[account];
  trophyprogs=<|"iname"->#["iname"],"pts"->#["pts"]+trophytime,"ymd"->#["ymd"]|>&/@trophyprogs;
  json=<|"ticket"->account["ticket"],"param"-><|"btlid"->account["battle_result"]["body"]["btlid"],"btlendparam"-><|"time"->0,"result"->"win","beats"->Table[1,number],"steals"-><|"items"->Table[0,number],"golds"->Table[0,number]|>,"missions"->{},"inputs"->{},"token"->token|>,"fuids"->{},"trophyprogs"->trophyprogs|>|>;
  body=ExportString[json,"RawJSON","Compact"->True];
  account["battle_result"]=Null;
  account["room"]=Null;
  tagatameRequest[account,"/btl/multi/end",body]
];
SetAttributes[multiBtlEnd, HoldFirst];

shopUpdate[account_, shop_,cost_]:=Block[
{},
account["ticket"]++;
tagatameRequest[account,"/shop/update",
ExportString[
<|"ticket"->account["ticket"],"param"-><|"iname"->shop,"costiname"->cost|>|>,
"RawJSON","Compact"->True]]
];
SetAttributes[shopUpdate, HoldFirst];

buy[account_, shop_,id_,num_:1]:=Block[
{},
account["ticket"]++;
tagatameRequest[account,"/shop/event/buy",
ExportString[
<|"ticket"->account["ticket"],"param"-><|"shopName"->shop,"id"->id,"buynum"->num|>|>,
"RawJSON","Compact"->True]]
];
SetAttributes[buy, HoldFirst];

trophyExec[account_,iname_,pts_,day_]:=Block[
{today},
account["ticket"]++;
today=FromDigits@StringJoin[IntegerString[#,10,2]&/@DateValue[{"Year","Month","Day"}]];
tagatameRequest[account,"/trophy/exec",
ExportString[<|"ticket"->account["ticket"],"param"-><|"trophyprogs"->{<|"iname"->iname,"pts"->{1},"ymd"->day,"rewarded_at"->today|>}|>|>,"RawJSON","Compact"->True]]
];
SetAttributes[trophyExec, HoldFirst];

multiBtlRetire[]:=Block[
  {json,number,body},
  ticket++;
  number=Length[battleResult["btlinfo"]["drops"]];
  json=<|"ticket"->ticket,"param"-><|"btlid"->battleId,"btlendparam"-><|"time"->0,"result"->"retire","beats"->Table[0,number],"steals"-><|"items"->Table[0,number],"golds"->Table[0,number]|>,"missions"->{},"inputs"->{},"token"->multiToken|>,"fuids"->{},"trophyprogs"->{}|>|>;
  body=ExportString[json,"RawJSON","Compact"->True];
  tagatameRequest["/btl/multi/end",body]
];

registerAccount[]:=Module[
{secretkey,deviceid,account},
secretkey=CreateUUID[];
deviceid=tagatameRequestWithoutSSID["/gauth/register","{\"ticket\":\"0\",\"access_token\":\"\",\"param\":{\"udid\":\"\",\"secret_key\":\""<>secretkey<>"\",\"idfv\":\""<>CreateUUID[]<>"\",\"idfa\":\""<>CreateUUID[]<>"\"}}"]["body"]["device_id"];
account=accountLogin[deviceid,secretkey];
tagatameRequest[account,"/playnew","{\"ticket\":4,\"param\":{\"permanent_id\":\""<>StringDelete[CreateUUID[],"-"]<>"\"}}"];
<|"device_id"->deviceid,"secret_key"->secretkey|>
];

getQuests[account_]:=Block[
  {body},
  account["ticket"]++;
  body="{\"ticket\":"<>ToString[account["ticket"]]<>",\"param\":{\"event\":1}}";
  tagatameRequest[account,"/btl/com",body]["body"]["quests"]
];
SetAttributes[getQuests, HoldFirst];

End[]
EndPackage[]
