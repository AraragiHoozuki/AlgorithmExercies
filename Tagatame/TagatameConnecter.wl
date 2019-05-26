(* ::Package:: *)

BeginPackage["TAGATAME`"];
Unprotect @@ Names["TAGATAME`*"];
ClearAll @@ Names["TAGATAME`*"];


(*Public Names*)
GetMainAccountToken::usage="GetMainAccountToken[] get the accesstoken of main account";
GetAccessToken::usage="GetAccessToken[deviceid, secretkey]";
CheckVer::usage="CheckVer[token, deviceid] ";
LoadMasterParam::usage="LoadMasterParam[] Load MasterParam and returns an associatio";

Trophy::usage="Trophy[token,deviceid,ticket,iname] execute trophy by iname";
RaidStart::usage="RaidStart[token, deviceid, ticket] start a raid";
RaidEnd::usage="RaidEnd[token,deviceid,ticket,raidinfo,hp] end a raid";

BattleRetire::usage="BattleRetire[token,deviceid,ticket, number,missiont:3]";


RijndaelDecrypt::usage="RijndaelDecrypt[bytes, key, iv]";
GenerateDLCKey::usage="GenerateDLCKey[token, deviceid, path]";


Begin["`Private`"];
Needs["NETLink`"];


(*Basic Functions*)
HexString2Bytes[string_]:=FromDigits[#,16]&/@StringPartition[StringDelete[string,{" ","\n"}],2]
Bytes2HexString[bytes_]:=StringRiffle[IntegerString[#,16]&/@bytes," "]
getTime[]:=QuantityMagnitude[DateObject[Now,"Second"]-DateObject[{2017,8,11,12,12,03},"Second"]]+1502424733-10


(*Constants*)


Host="alchemist.gu3.jp";
AssetHost="alchemist-dlc2.gu3.jp";
AssetVer="20190524_b268b220f92e85b44b90c2814c2e5d35972444ef_566f2";
AppVer="930d9d79:wd";
MasterDigest;

today=FromDigits@StringJoin[IntegerString[#,10,2]&/@DateValue[LocalTime[\!\(\*
NamespaceBox["LinguisticAssistant",
DynamicModuleBox[{WolframAlphaClient`Private`query$$ = "Tokyo", WolframAlphaClient`Private`boxes$$ = TemplateBox[{"\"Tokyo\"", RowBox[{"Entity", "[", RowBox[{"\"City\"", ",", RowBox[{"{", RowBox[{"\"Tokyo\"", ",", "\"Tokyo\"", ",", "\"Japan\""}], "}"}]}], "]"}], "\"Entity[\\\"City\\\", {\\\"Tokyo\\\", \\\"Tokyo\\\", \\\"Japan\\\"}]\"", "\"city\""}, "Entity"], WolframAlphaClient`Private`allassumptions$$ = {{"type" -> "Clash", "word" -> "Tokyo", "template" -> "Assuming \"${word}\" is ${desc1}. Use as ${desc2} instead", "count" -> "2", "Values" -> {{"name" -> "City", "desc" -> "a city", "input" -> "*C.Tokyo-_*City-"}, {"name" -> "AdministrativeDivision", "desc" -> "an administrative division", "input" -> "*C.Tokyo-_*AdministrativeDivision-"}}}}, WolframAlphaClient`Private`assumptions$$ = {}, WolframAlphaClient`Private`open$$ = {1, 2}}, 
DynamicBox[ToBoxes[AlphaIntegration`LinguisticAssistantBoxes["", 1, Dynamic[WolframAlphaClient`Private`query$$], Dynamic[WolframAlphaClient`Private`boxes$$], Dynamic[WolframAlphaClient`Private`allassumptions$$], Dynamic[WolframAlphaClient`Private`assumptions$$], Dynamic[WolframAlphaClient`Private`open$$]], StandardForm],
ImageSizeCache->{160., {9., 22.}},
TrackedSymbols:>{WolframAlphaClient`Private`query$$, WolframAlphaClient`Private`boxes$$, WolframAlphaClient`Private`allassumptions$$, WolframAlphaClient`Private`assumptions$$, WolframAlphaClient`Private`open$$}],
DynamicModuleValues:>{},
UndoTrackedVariables:>{WolframAlphaClient`Private`open$$}],
BaseStyle->{"Deploy"},
DeleteWithContents->True,
Editable->False,
SelectWithContents->True]\)],{"Year","Month","Day"}]];


(*MessagePack*)
LoadNETAssembly["D:\\MMALab\\Tagatame\\Connecter\\MessagePack.dll"];
LoadNETType["MessagePack.MessagePackSerializer"];


(*SymmetricEncryption*)
LoadNETAssembly["D:\\MMALab\\Tagatame\\Connecter\\mscorlib.dll"];
LoadNETType["System.Security.Cryptography.SHA256Managed"];
LoadNETType["System.Security.Cryptography.RijndaelManaged"];

DlcSharedKey=HexString2Bytes["9C 72 85 EB 20 F9 7C 1E 73 0D B9 85 8E BA 09 EA"];
AppSharedKey=HexString2Bytes["5F 3E 18 C9 C7 D7 43 E8 C7 0B 55 DD ED C8 3B C9"];

GenerateDLCKey[token_, deviceid_, path_]:=Block[
{sha256,bytes},
sha256=NETNew["System.Security.Cryptography.SHA256Managed"];
bytes=Join[
DlcSharedKey,
ToCharacterCode[path,"ASCII"],
ToCharacterCode[token,"ASCII"],
ToCharacterCode[deviceid,"ASCII"]
];
(sha256@ComputeHash[bytes])[[1;;16]]
]

GenerateAPPKey[token_, deviceid_, path_, noeks_:False]:=Block[
{sha256,bytes},
sha256=NETNew["System.Security.Cryptography.SHA256Managed"];
bytes=Join[
AppSharedKey,
ToCharacterCode[path,"ASCII"],
If[noeks,{},ToCharacterCode[token,"ASCII"]],
If[noeks, {},ToCharacterCode[deviceid,"ASCII"]]
];
(sha256@ComputeHash[bytes])[[1;;16]]
]

(*de/en crypt  msgpack bytes*)
RijndaelDecrypt[bytes_, key_, iv_: 0]:=Block[
{managed},
managed=NETNew["System.Security.Cryptography.RijndaelManaged"];
managed@KeySize=16^^80;
managed@BlockSize=16^^80;
If[iv==0, managed@IV=bytes[[1;;16]], managed@IV=iv];
managed@Key=key;
(managed@CreateDecryptor[])@TransformFinalBlock[bytes,16^^10, Length[bytes]-16^^10]
]

RijndaelEncrypt[bytes_, key_]:=Block[
{managed,output},
managed=NETNew["System.Security.Cryptography.RijndaelManaged"];
managed@KeySize=16^^80;
managed@BlockSize=16^^80;
managed@Key=key;
output=(managed@CreateEncryptor[])@TransformFinalBlock[bytes, 0, Length[bytes]];
Join[managed@IV, output]
]


(*Native Pluguin*)
NativePluginPath = "D:\\MMALab\\Tagatame\\Connecter\\NativePlugin.dll";
LoadNETType["System.Runtime.InteropServices.Marshal"];
decompressFile=DefineDLLFunction["DecompressFile", NativePluginPath, "IntPtr", {"string","out int"},MarshalStringsAs->"ANSI"];
FreePtr=DefineDLLFunction["FreePtr", NativePluginPath, "void", {"IntPtr"},MarshalStringsAs->"ANSI"];
LoadMasterParam[]:=Block[{size=0, array, ptr, key,sha256},
ptr = decompressFile["D:\\PCGAME\\tagatame\\new_win32\\4a6996fe", size];
array=NETNew["System.Byte[]",size];
Marshal`Copy[ptr,array,0,size];
FreePtr[ptr];
array=NETObjectToExpression[array];
sha256=NETNew["System.Security.Cryptography.SHA256Managed"];
key=(sha256@ComputeHash[AppSharedKey])[[1;;16]];
array=RijndaelDecrypt[array, key, HexString2Bytes[MasterDigest]];
array[[1;;16]]=HexString2Bytes["DF 00 00 00 77 A7 41 62 69 6C 69 74 79 DD 00 00"];
ImportString[FromCharacterCode[ToCharacterCode[MessagePackSerializer`ToJson[array],"UTF-8"]],"RawJSON"]
]


(* Basic Requests*)
GetMainAccountToken[token_:"d68085d66a1aae13a1d250ca1326664f"]:=Block[
  {req,body,guid,json},
  guid=StringDelete[CreateUUID[],"-"];
  body=  "{
    \"dmm_onetime_token\": \""<>token<>"\",
    \"dmm_viewer_id\": 198045004,
    \"idfa\": \"\",
    \"idfv\": \"\",
    \"udid\": \"\"
}";
  req=HTTPRequest[
    <|"Scheme"->"https",
      "Domain"->"production-alchemist.nativebase.gu3.jp",
      "Path"->"/dmm-auth-proxy/alchemist/get_access_token",
      Method -> "POST",
      "Headers"->{
        "User-Agent"->"UnityPlayer/5.3.6p1 (http://unity3d.com)",
        "Host"->"production-alchemist.nativebase.gu3.jp",
        "Accept"->"*/*",
        "Accept-Encoding"->"identity",
        "Content-Type"->"application/json; charset=utf-8",
        "X-GUMI-CLIENT"->"gscc ver.0.1",
        "X-GUMI-DEVICE-OS"->"windows",
        "X-GUMI-REQUEST-ID"->guid,
        "X-GUMI-STORE-PLATFORM"->"dmmgamesstore",
        "X-GUMI-TRANSACTION"-> guid,
        "X-Gumi-Game-Environment"->"production",
        "X-Unity-Version"-> "5.6.6f2",
        "X-Gumi-User-Agent"->"{\"device_model\":\"Alienware 15\",\"device_vendor\":\"Alienware\",\"os_info\":\"Windows 10 (10.0.0) 64bit\",\"cpu_info\":\"Intel(R) Core(TM) i7-4710HQ CPU @ 2.50GHz\",\"memory_size\":\"16.269GB\"}",
        "Content-Length"->Length@ImportString[body,{"Binary","Byte"}]
        },
      "Body"-> body,
VerifySecurityCertificates -> False |>];
json=URLRead[req,"Body"];
json=ImportString[json,"RawJSON"];
json["access_token"]
]

CheckVer[token_, deviceid_]:=Block[
  {req,body,guid,resp},
  guid=StringDelete[CreateUUID[],"-"];
  body="{\"ver\":\""<>AppVer<>"\"}";
  req=HTTPRequest[
    <|"Scheme"->"https",
      "Domain"->Host,
      "Path"->"/chkver2",
      Method -> "POST",
      "Headers"->{
        "User-Agent"->"UnityPlayer/5.6.6f2 (UnityWebRequest/1.0, libcurl/7.51.0-DEV)",
        "Host"->Host,
        "Accept"->"*/*",
        "Accept-Encoding"->"identity",
        "Authorization"->"gauth " <> token,
        "Content-Encoding"->"identity",
        "Content-Type"->"application/octet-stream+jhotuhiahanoatuhinga+fakamunatanga",
        "x-app-ver"->AppVer,
        "x-asset-ver"->AssetVer,
        "X-GUMI-CLIENT"->"gscc ver.0.1",
        "X-GUMI-DEVICE-OS"->"windows",
        "X-GUMI-REQUEST-ID"->guid,
        "X-GUMI-STORE-PLATFORM"->"dmmgamesstore",
        "X-GUMI-TRANSACTION"-> guid,
        "X-Unity-Version"-> "5.6.6f2",
        "X-Gumi-User-Agent"->"{\"device_model\":\"Alienware 15\",\"device_vendor\":\"Alienware\",\"os_info\":\"Windows 10 (10.0.0) 64bit\",\"cpu_info\":\"Intel(R) Core(TM) i7-4710HQ CPU @ 2.50GHz\",\"memory_size\":\"16.269GB\"}",
        "Content-Length"->Length@ImportString[body,{"Binary","Byte"}]
        },
      "Body"-> body,
VerifySecurityCertificates -> False |>];
resp=URLRead[req,"BodyBytes"];
resp=RijndaelDecrypt[resp, GenerateAPPKey[token, deviceid, "/chkver2",True]];
resp=ExportString[resp,"Binary"];
resp=ImportString[resp,"RawJSON"];
AssetVer=resp[["body","environments","alchemist","assets"]];
MasterDigest=resp[["body","environments","alchemist","master_digest"]];
resp
]

GetAccessToken[deviceid_:"a7b429b5-88a0-4a5b-9082-495f526dc8a0", secretkey_:"ef9ac5ff-0818-46e0-9837-e1b20fd47b5a"]:=Block[
{req, json,body,path="/gauth/accesstoken",guid,resp},
guid=StringDelete[CreateUUID[],"-"];
json=<|"ticket"->0,"access_token"->"",  "param"-><|"device_id"->deviceid,"secret_key"->secretkey|>|>;
json=ExportString[json,"RawJSON", "Compact"->True];
body=MessagePackSerializer`FromJson[json];
body= RijndaelEncrypt[body, GenerateAPPKey["", deviceid, path,True]];
 req=HTTPRequest[
    <|"Scheme"->"https",
      "Domain"->Host,
      "Path"->path,
      Method -> "POST",
      "Headers"->{
        "User-Agent"->"UnityPlayer/5.6.6f2 (UnityWebRequest/1.0, libcurl/7.51.0-DEV)",
        "Host"->Host,
        "Accept"->"*/*",
        "Accept-Encoding"->"identity",
        "Content-Encoding"->"identity",
        "Content-Type"->"application/octet-stream+jhotuhiahanoatuhinga+fakamunatanga",
        "x-app-ver"->AppVer,
        "x-asset-ver"->AssetVer,
        "X-GUMI-CLIENT"->"gscc ver.0.1",
        "X-GUMI-DEVICE-OS"->"windows",
        "X-GUMI-REQUEST-ID"->guid,
        "X-GUMI-STORE-PLATFORM"->"dmmgamesstore",
        "X-GUMI-TRANSACTION"-> guid,
        "X-Unity-Version"-> "5.6.6f2",
        "X-Gumi-User-Agent"->"{\"device_model\":\"Alienware 15\",\"device_vendor\":\"Alienware\",\"os_info\":\"Windows 10 (10.0.0) 64bit\",\"cpu_info\":\"Intel(R) Core(TM) i7-4710HQ CPU @ 2.50GHz\",\"memory_size\":\"16.269GB\"}",
        "Content-Length"->Length[body]
        },
      "Body"-> body,
VerifySecurityCertificates -> False |>];
resp=URLRead[req,"BodyBytes"];
Print[FromCharacterCode[resp,"UTF-8"]];
resp=RijndaelDecrypt[resp, GenerateAPPKey["", deviceid, path,True]];
resp=MessagePackSerializer`ToJson[resp];
resp
]

(*body should be in json, and will be compressed to msgpack and encrypt by this function*)
BasicRequest[token_, deviceid_, bytes_ ,path_,contenttype_,useMessagePack_:True]:=Block[
  {req,guid,body,resp},
  guid=StringDelete[CreateUUID[],"-"];
  If[useMessagePack,body=MessagePackSerializer`FromJson[bytes],body=ToCharacterCode[bytes,"ASCII"]];
  body=RijndaelEncrypt[body, GenerateDLCKey[token, deviceid, path]];
  req=HTTPRequest[
    <|"Scheme"->"https",
      "Domain"->Host,
      "Path"->path,
      Method -> "POST",
      "Headers"->{
        "User-Agent"->"UnityPlayer/5.6.6f2 (UnityWebRequest/1.0, libcurl/7.51.0-DEV)",
        "Accept"->"*/*",
        "Accept-Encoding"->"identity",
        "Authorization"->"gauth " <> token,
        "Content-Encoding"->"identity",
        "Content-Type"->contenttype,
        "x-app-ver"->AppVer,
        "x-asset-ver"->AssetVer,
        "X-GUMI-CLIENT"->"gscc ver.0.1",
        "X-GUMI-DEVICE-OS"->"windows",
        "X-Gumi-Game-Environment"->"production",
        "X-GUMI-REQUEST-ID"->guid,
        "X-GUMI-STORE-PLATFORM"->"dmmgamesstore",
        "X-GUMI-TRANSACTION"-> guid,
        "X-Unity-Version"-> "5.6.6f2",
        "X-Gumi-User-Agent"->"{\"device_model\":\"Alienware 15\",\"device_vendor\":\"Alienware\",\"os_info\":\"Windows 10 (10.0.0) 64bit\",\"cpu_info\":\"Intel(R) Core(TM) i7-4710HQ CPU @ 2.50GHz\",\"memory_size\":\"16.269GB\"}",
        "Content-Length"->Length@body
        },
      "Body"-> body,
      VerifySecurityCertificates -> False  |>];
resp=URLRead[req,"BodyBytes"];
resp=RijndaelDecrypt[resp, GenerateDLCKey[token, deviceid, path]];
If[useMessagePack,resp=MessagePackSerializer`ToJson[resp],resp=FromCharacterCode[resp]];
resp
]



(*Advanced Requests*)
Trophy[token_,deviceid_,ticket_,iname_]:=Block[
{body},
body=ExportString[<|"ticket"->ticket,"param"-><|"trophyprogs"->{<|"iname"->iname,"pts"->{1},"ymd"->today,"rewarded_at"->today|>}|>|>,"RawJSON","Compact"->True];
BasicRequest[token, deviceid, body, "/trophy/exec","application/octet-stream+karerepokai+fakamunatanga"]
]

RaidStart[token_, deviceid_, ticket_]:=Block[
  {json,raidboss,areaid,bossid,round,body,result}, 
raidboss=BasicRequest[token,deviceid,"{\"ticket\":"<>ToString[ticket]<>"}","/raidboss","application/octet-stream+jhotuhiahanoatuhinga+fakamunatanga",False];
raidboss=ImportString[raidboss,"RawJSON"][["body"]];
{areaid,round}=Values[raidboss[[{"area_id","round"}]]];

If[KeyExistsQ[raidboss[["raidboss_current"]],"boss_info"],
bossid=raidboss[["raidboss_current","boss_info","boss_id"]];,
bossid=BasicRequest[token,deviceid,"{\"ticket\":"<>ToString[ticket]<>"}","/raidboss/select","application/octet-stream+jhotuhiahanoatuhinga+fakamunatanga",False];
bossid=ImportString[bossid,"RawJSON"][["body","raidboss_current","boss_info","boss_id"]];
];

json=<|"param"-><|"area_id"->areaid,"boss_id"->bossid,"round"->round,"uid"->""|>,"ticket"->ticket|>;
body=ExportString[json,"RawJSON","Compact"->True] ;
result=BasicRequest[token,deviceid,body,"/raidboss/btl/req","application/octet-stream+jhotuhiahanoatuhinga+fakamunatanga",False];
(*return raid info, will be need to end raid*)
ImportString[result,"RawJSON"]["body"]
]

RaidEnd[token_,deviceid_,ticket_,raidinfo_,hp_:0]:=Block[
{json,number,info,body},
info=raidinfo;
json=<|"ticket"->ticket,"param"-><|"btlid"->info["btlid"],"status"->"win","boss_info"-><|"no"->info["btlinfo"]["no"],"boss_id"->info["btlinfo"]["boss_info"]["boss_id"],"round"->info["btlinfo"]["round"],"current_hp"->hp,"start_time"->0,"is_reward"->0,"is_timeover"->0,"is_rescue_damage_zero"->0,"is_beat_resucue"->0|>,"btlendparam"-><|"inputs"->{}|>|>|>;
body=ExportString[json,"RawJSON","Compact"->True];
BasicRequest[token,deviceid,body,"/raidboss/btl/end","application/octet-stream+jhotuhiahanoatuhinga+fakamunatanga",False]
]

BattleStart[token_,deviceid_,ticket_,questid_]:=Block[
  {json,body,result},
  json=<|"ticket"->ticket,"param"-><|"iname"->questid,"partyid"->0,"req_at"->getTime[],"btlparam"-><|"help"-><|"fuid"->""|>|>,"location"-><|"lat"->0,"lng"->0|>|>|>;
  body=ExportString[json,"RawJSON","Compact"->True] ;
  result=BasicRequest[token,deviceid,body,"/btl/com/req","application/octet-stream+jhotuhiahanoatuhinga+fakamunatanga", False];
];

BattleRetire[token_,deviceid_,ticket_, number_,missiont_:3]:=Block[
{json,body,result},
json=<|"ticket"->ticket,"param"-><|"btlid"->15134380,"btlendparam"-><|"time"->0,"result"->"retire","beats"->Table[0,number],"steals"-><|"items"->Table[0,number],"golds"->Table[0,number]|>,"missions"->Table[0,missiont],"inputs"->{}|>,"trophyprogs"->{}|>|>;
body=ExportString[json,"RawJSON","Compact"->True];
BasicRequest[token,deviceid,body,"/btl/com/end","application/octet-stream+jhotuhiahanoatuhinga+fakamunatanga",False]
];


End[];
EndPackage[];
