;Yaml v1.0.4 requires AutoHotkey v2.106+
Yaml(TextFileObject,Yaml:=0){
  If IsObject(TextFileObject)
    return Dump(TextFileObject,Yaml) ; dump object to yaml string
  else If FileExist(TextFileObject)
    return Load(FileRead(file),Yaml) ; load yaml from file
  else return Load(TextFileObject,Yaml) ; load object from yaml string
  getline(p,ByRef LF:=""){ ; get line and advance pointer to next line
    return !p||!NumGet(p,"UShort")?0:(str:=StrSplit(StrGet(P),"`n","`r",2)).Length?(p+=StrLen(LF:=str[1])*2,p+=!NumGet(p,"UShort") ? 0 : NumGet(p,"USHORT")=13?4:2):0
  }
  Load(txt,Y:=0){ ; convert yaml to object
    P:=&txt,A:=Map()
	If SubStr(LTrim(txt," `t"),1,1)="{" ; create json map object
        return (O:=[{}],YamlObject(O[1],(&txt)+InStr(txt,"{")*2,1),O)
	else if SubStr(LTrim(txt," `t"),1,1)="[" ; create json sequence object
		return (O:=[[]],YamlArray(O[1],(&txt)+InStr(txt,"[")*2,1),O)
    D:=[],I:=[]
    Loop 1000
      D.Push(0),I.Push(0)
    While P:=getline(LP:=P,LF){
      if (InStr(LF,"---")=1&&!Y)||(InStr(LF,"---")=1&&(Y.Push(""),D[1]:=0,_L:=_LL:=O:=_Q:=_K:=_S:=_T:=_V:="",1))||(InStr(LF,"...")=1&&NEWDOC:=1)||(LF="")||RegExMatch(LF,"^\s+$")
        continue
      else if NEWDOC&&MsgBox("Error, document ended but new document not specified: " LF)
        Exit
      if RegExMatch(LF,"^\s*#")||InStr(LF,"``%")=1 ; Comments, tag, document start/end or empty line, ignore
        continue
      else If _C || (_S&&SubStr(LF,1,LL*2)=I2S(LL+1)) || (V&&!(K&&_.SEQ)&&SubStr(LF,1,LL*2)=I2S(LL+1)){ ; Continuing line incl. scalars
        if _Q&&!_K{ ; Sequence
          If D[L].Length && IsObject(VC:=D[L].Pop()) && MsgBox("Error: Malformed inline YAML string") ; Error if previous value is an object
            Exit
          else D[L].Push(VC (VC?(_S=">"?" ":"`n"):"") _CE:=LTrim(LF,"`t ")) ; append value to previous item
        } else if IsObject(VC:=D[L].%K%) && MsgBox("Error: Malformed inline YAML string") ; Error if previous value is an object
          Exit
        else D[L].%K%:=VC (VC?(_S=">"?" ":"`n"):"") _CE:=LTrim(LF,"`t ") ; append value to previous item
          continue
      } else if _C&&(SubStr(_CE,-1)!=_C)&&MsgBox("Error: unexpected character near`n" (_Q?D[L][D[L].Length]:D[L].%K%)) ; else check if quoted value was ended with a quote
        Exit
      else _C:="" ; reset continuation
      If (CM:=InStr(LF," #"))&&!RegExMatch(LF,".*[`"'].*\s\#.*[`"'].*") ; check for comments and remove
        LF:=SubStr(LF,1,CM-1)
      ; Split line into yaml elements
      RegExMatch(LF,"S)^(?<LVL>\s+)?(?<SEQ>-\s)?(?<KEY>`".+`"\s*:\s|'.+'\s*:\s|[^:`"'\{\[]+\s*:\s)?\s*(?<SCA>[\|\>][+-]?)?\s*(?<TYP>!!\w+)?\s*(?<AGET>\*[^\s\t]+)?\s*(?<ASET>&[^\s\t]+)?\s*(?<VAL>`".+`"|'.+'|.+)?\s*$",_),L:=LL:=S2I(_.LVL),Q:=_.SEQ,K:=_.KEY,S:=_.SCA,T:=SubStr(_.TYP,3),V:=UnQuote(_.VAL),V:= V is "Integer"?V+0:V,VQ:=InStr(".''.`"`".", "." SubStr(LTrim(_.VAL," `t"),1,1) SubStr(RTrim(_.VAL," `t"),-1) ".")
      if L>1{
        if LL=_LL
          L:=_L
        else if LL>_LL
          I[LL]:=L:=_L+1
        else if LL<_LL
          if !I[LL]&&MsgBox("Error, indentation problem: " LF)
            Exit
          else L:=I[LL]
       }
      if Trim(_.Value()," `t")="-" ; empty sequence not cached by previous line
        V:="",Q:="-"
      else if !K&&V&&!Q ; only a value is catched, convert to key
        K:=V,V:=""
      If !Q&&SubStr(RTrim(K," `t"),-1)!=":" ; not a sequence and key is missing :
        if L>_L&&(D[_L].%_K%:=K,LL:=_LL,L:=_L,K:=_K,Q:=_Q,_S:=">")
          continue
        else (MsgBox("Error, invalid key:`n" LF),Exit())
      else if K!="" ; trim key if not empty
        K:=UnQuote(RTrim(K,": "))
      Loop _L ? _L-L : 0 ; remove objects in deeper levels created before
        D[L+A_Index]:=0,I[L+A_Index]:=0
      if !VQ&&!InStr("'`"",_C:=SubStr(LTrim(_.VAL," `t"),1,1)) ; check if value started with a quote and was not closed so next line continues
        _C:=""
      if _L!=L && !D[L] ; object in this level not created yet
        if L=1{ ; first level, use or create main object
          if Y&&Type(Y[Y.Length])!="String"&&((Q&&Type(Y[Y.Length])!="Array")||(!Q&&Type(Y[Y.Length])="Array"))&&MsgBox("Mapping Item and Sequence cannot be defined on the same level:`n" LF) ; trying to create sequence on the same level as key or vice versa
            Exit
          else D[L]:=Y ? (Type(Y[Y.Length])="String"?(Y[Y.Length]:=Q?[]:{}):Y[Y.Length]) : (Y:=Q?[[]]:[{}])[1]
        } else if !_Q&&Type(D[L-1].%_K%)=(Q?"Array":"Object") ; use previous object
          D[L]:=D[L-1].%_K%
        else D[L]:=O:=Q?[]:{},_A?A[_A]:=O:"",_Q ? D[L-1].Push(O) : D[L-1].%_K%:=O,O:="" ; create new object
      _A:="" ; reset alias
      if Q&&K ; Sequence containing a key, create object
        D[L].Push(O:={}),D[++L]:=O,Q:=O:="",LL+=1
      If (Q&&Type(D[L])!="Array"||!Q&&Type(D[L])="Array")&&MsgBox("Mapping Item and Sequence cannot be defined on the same level:`n" LF) ; trying to create sequence on the same level as key or vice versa
        Exit
      if T="binary"{ ; !!binary
        O:=BufferAlloc(StrLen(V)//2),PBIN:=O.Ptr
        Loop Parse V
          If (""!=h.=A_LoopField) && !Mod(A_Index,2)
            NumPut("0x" h,PBIN,A_Index/2-1,"UChar"),h:=""
      } else if T="set"&&MsgBox("Error, Tag set is not supported") ; tag !!set is not supported
        Exit 
      else V:=T="int"||T="float"?V+0:T="str"?V "":T="null"?"":T="bool"?(V="true"?true:V="false"?false:V):V ; tags !!int !!float !!str !!null !!bool - else seq map omap ignored
      if _.ASET
        A[_A:=SubStr(_.ASET,2)]:=V
      if _.AGET
        V:=A[SubStr(_.AGET,2)]
      else If SubStr(LTrim(V," `t"),1,1)="{" ; create json map object
        O:={},_A?A[_A]:=O:"",P:=(YamlObject(O,LP+InStr(LF,V)*2,L))
      else if SubStr(LTrim(V," `t"),1,1)="[" ; create json sequence object
        O:=[],_A?A[_A]:=O:"",P:=(YamlArray(O,LP+InStr(LF,V)*2,L))
      if Q ; push sequence value into an object
        (V ? D[L].Push(O?O:S?"":V) : 0)
      else D[L].%K%:=O?O:D[L].HasOwnProp(K)?D[L].%K%:S?"":V ; add key: value into object
      if !Q&&V ; backup yaml elements
        _L:=L,_LL:=LL,O:=_Q:=_K:=_S:=_T:=_V:="" ;_L:=
      else _L:=L,_LL:=LL,_Q:=Q,_K:=K,_S:=S,_T:=T,_V:=V,O:=""
    }
    if Y&&Type(Y[Y.Length])="String"
      Y.Pop()
    return Y
  }
  UniChar(s){ ; convert unicode and special characters
    static m:=Map("a","`a","b","`b","t","`t","n","`n","v","`v","f","`f","r","`r","e",Chr(0x1B))
    Loop Parse (e:=0,s),"\"
      If (A_Index=1&&A_LoopField!=""&&v:=A_LoopField) || (e && (e:=0,v.="\" A_LoopField)) || (A_LoopField=""&&e:=1)
        Continue
      else v .= RegexMatch( t := (t:=InStr("ux",SubStr(A_LoopField,1,1)) ? SubStr(A_LoopField,1,RegExMatch(A_LoopField,"^[ux]?([\dA-F]{4})?([\dA-F]{2})?\K")-1) : SubStr(A_LoopField,1,1)) == "N" ? "\x85" : t == "P" ? "\x2029" : t = 0 ? "\x0" : t == "L" ? "\x2028" : t == "_" ? "\xA0" : t ,"i)^[ux][\da-f]+$") ? Chr(Abs("0x" SubStr(t,2))) : m.has(t) ? m[t] : t
          ,v .= InStr("ux",SubStr(A_LoopField,1,1)) ? SubStr(A_LoopField,RegExMatch(A_LoopField,"^[ux]?([\dA-F]{4})?([\dA-F]{2})?\K")) : SubStr(A_LoopField,2)
    return v
  }
  CharUni(s){ ; convert text to unicode notation
    static ascii:=Map("\","\","`a","a","`b","b","`t","t","`n","n","`v","v","`f","f","`r","r",Chr(0x1B),"e","`"","`"",Chr(0x85),"N",Chr(0x2029),"P",Chr(0x2028),"L","","0",Chr(0xA0),"_")
    If (!(v:="") && !RegexMatch(s,"[\x{007F}-\x{FFFF}]")){
      Loop Parse, s
        v .= ascii.Has(A_LoopField) ? "\" ascii[A_LoopField] : A_LoopField
      return v
    }
    Loop Parse, s
      v .= ascii.Has(A_LoopField) ? "\" ascii[A_LoopField] : Ord(A_LoopField)<128 ? A_LoopField : "\u" format("{1:.4X}",Ord(A_LoopField))
    return v
  }
  EscIfNeed(s){ ; check if escaping needed and convert to unicode notation
    If s=""
      return "''"
    else If RegExMatch(s,"m)[\{\[`"'\r\n]|:\s|,\s|\s#")||RegExMatch(s,"^[\s#\\\-:>]")||RegExMatch(s,"m)\s$")||RegExMatch(s,"m)[\x{7F}-\x{7FFF}]"){
      return ("`"" . CharUni(s) . "`"")
    } else return s
  }
  Dump(O:="",J:=0){
    if Type(O)!="Array"
      dump.= DumpSub(O,J)
    else
      for K,V in O
        dump.="---`n" DumpSub(V,J) "`n"
    return dump
  }
  DumpSub(O:="",J:=0,R:=0,Q:=0){ ; convert object to yaml string
    static M1:="{",M2:="}",S1:="[",S2:="]",N:="`n",C:=", ",S:="- ",E:="",K:=": "
    If (t:=type(O))="Array"{
      dump:=J<1&&!R?S1:""
      for key, value in O{
        if Type(value)="Buffer"{
          Loop (VAL:="",PTR:=value.Ptr,value.size)
            VAL.=format("{1:.2X}",NumGet(PTR+A_Index-1,"UCHAR"))
          value:="!!binary " VAL,F:=E
        } else
          F:=IsObject(value)?(Type(value)="Array"?"S":"M"):E
		Z:=Type(value)="Array"&&value.Length=0?"[]":((Type(value)="Map"&&value.count=0)||(Type(value)="Object"&&ObjOwnPropCount(value)=0))?"{}":""
        If J<=R
          dump.=(J<(R+1)*-1?"`n" I2S(R+2):"") (F?(%F%1 (Z?"":DumpSub(value,J,R+1,F)) %F%2):EscIfNeed(value)) ((Type(O)="Array"&&O.Length=A_Index) ? E : C)
        else if ((dump:=dump N I2S(R+1) S)||1)&&F
            dump.= Z?Z:(J<=(R+1)?%F%1:E) DumpSub(value,J,R+1,F) (J<=(R+1)?%F%2:E)
        else dump .= EscIfNeed(value)
      }
    } else {
      dump:=J<1&&!R?M1:""
      for key, value in Type(O)="Map"?(Y:=1,O):O.OwnProps(){
        if Type(value)="Buffer"{
          Loop (VAL:="",PTR:=value.Ptr,value.size)
            VAL.=format("{1:.2X}",NumGet(PTR+A_Index-1,"UCHAR"))
          value:="!!binary " VAL,F:=E
        } else
          F:=IsObject(value)?(Type(value)="Array"?"S":"M"):E
		Z:=Type(value)="Array"&&value.Length=0?"[]":((Type(value)="Map"&&value.count=0)||(Type(value)="Object"&&ObjOwnPropCount(value)=0))?"{}":""
        If J<=R
          dump.=(J<(R+1)*-1?"`n" I2S(R+2):"") (Q="S"&&A_Index=1?M1:E) EscIfNeed(key) K (F?(%F%1 (Z?"":DumpSub(value,J,R+1,F)) %F%2):EscIfNeed(value)) (Q="S"&&A_Index=(Y?O.count:ObjOwnPropCount(O))?M2:E) (J!=0||R?(A_Index=(Y?O.count:ObjOwnPropCount(O))?E:C):E)
        else If ((dump:=dump N I2S(R+1) EscIfNeed(key) K)||1)&&F
          dump.= Z?Z:(J<=(R+1)?%F%1:E) DumpSub(value,J,R+1,F) (J<=(R+1)?%F%2:E)
        else dump .= EscIfNeed(value)
        If J=0&&!R
          dump.= (A_Index<(Y?O.count:ObjOwnPropCount(O))?C:E)
      }
    }
	if J<0&&J<R*-1
	  dump.= "`n" I2S(R+1)
    If R=0
      dump:=RegExReplace(dump,"^\R+") (J<1?(Type(O)="Array"?S2:M2):"")
    Return dump
  }
  S2I(s){ ; Convert Spaces to level, 1 = first level
    Loop Parse, (i:=0,s)
      i += (A_LoopField=A_Tab||!Mod(A_index,2)) ? 1 : 0
    Return i + 1
  }
  I2S(i){ ; Convert level to spaces
    Loop i-1
      s .= "  "
    Return s
  }
  UnQuote(s){ ; remove quotes
    return (t:=SubStr(s:=Trim(s," `t"),1,1) SubStr(s,-1)) = '""' ? UniChar(SubStr(s,2,-1)) : t = "''" ? SubStr(s,2,-1) : s
  }
  YamlObject(O,P,L){ ; convert json map 
    v:=q:=k:=0,key:=val:=lf:=""
    While (""!=c:=Chr(NumGet(p,"UShort")))&&(InStr("`n`r",c)||""!=(s:=c)){
      if c="`n"||(c="`r"&&10=NumGet(p+2,"UShort")){
        if (q||(k&&s=":")||(!v&&!k&&(s=","||s=""))||InStr(Ltrim(StrGet(p+(c="`n"?2:4))," `t"),"}")=1) && P+=c="`n"?2:4
          continue
        else if MsgBox("Error: Malformed inline YAML string: " StrGet(p))
          Exit
      } else if !q&&(c=" "||c=A_Tab) && P+=2
        continue
      if !v&&(c='"'||c="'") && (q:=c,v:=1,P+=2)
        continue
      else if !v&&k&&(c="["||c="{") && (P:=c="[" ? YamlArray(O.%key%:=[],P+2,L) : YamlObject(O.%key%:={},P+2,L),key:="",k:=0,1)
        continue
      else if v&&!k&&((!q&&c=":")||(q&&q=c)) && (v:=0,key:=key is "number" ? key+0 : q ? UniChar(key) : Trim(key," `t"),k:=1,q:=0,P+=2)
        continue
      else if v&&k&&((!q&&c=",")||(q&&q=c)) && (v:=0,O.%key%:=val is "number" ? val+0 : q ? UniChar(val) : Trim(val," `t"),val:="",key:="",q:=0,k:=0,P+=2)
        continue
      else if !q&&c="}"&&(k&&v?(O.%key%:=val,1):1){
        if ((tp:=getline(P+2,lf))&&(NumGet(P+2,"UShort")=10||NumGet(P+4,"UShort")=10||(nl:=RegExMatch(lf,"^\s+?$"))||RegExMatch(lf,"^\s*[,\}\]]")))
          return nl?tp:P+2
        else if !tp
          return 0
        else if MsgBox("Error: Malformed inline YAML string: " StrGet(p))
          Exit
      } else if !v&& InStr(",: `t",c)&&P+=2
        continue
      else if !v&& (!k ? (key:=c) : val:=c,v:=1,P+=2)
        continue
      else if v&& (!k ? (key.=c) : val.=c,P+=2)
        continue
      else if MsgBox("Error undefined")
        Exit 
    }
    MsgBox("Error: unexpected end of YAML string: " StrGet(p))
    Exit
  }
  YamlArray(O,P,L){ ; convert json sequence
    v:=q:=c:=0,lf:=""
    While (""!=c:=Chr(NumGet(p,"UShort")))&&(InStr("`n`r",c)||""!=(s:=c)){
      if c="`n"||(c="`r"&&10=NumGet(p+2,"UShort")){
        if (q||(!v&&(s=","||s=""))||InStr(Ltrim(StrGet(p+(c="`n"?2:4))," `t"),"]")=1) && P+=c="`n"?2:4
          continue
        else if MsgBox("Error: Malformed inline YAML string: " StrGet(p))
          Exit
      } else if !q&&(c=" "||c=A_Tab) && P+=2
        continue
      if !v&&(c='"'||c="'") && (q:=c,v:=1,P+=2)
        continue
      else if !v&&(c="["||c="{") && (P:=c="[" ? YamlArray((O.Push(lf:=[]),lf),P+2,L) : YamlObject((O.Push(lf:={}),lf),P+2,L),lf:="",1)
        continue
      else if v&&((!q&&c=",")||(q&&c=q)) && (v:=0,O.Push(lf is "number" ? lf+0 : q ? UniChar(lf) : Trim(lf," `t")),q:=0,lf:="",P+=2)
        continue
      else if !q&&c="]"&&(v?(O.Push(Trim(lf," `t")),1):1){
        if ((tp:=getline(P+2,lf))&&(NumGet(P+2,"UShort")=10||NumGet(P+4,"UShort")=10||(nl:=RegExMatch(lf,"^\s+?$"))||RegExMatch(lf,"^\s*[,\}\]]")))
          return nl?tp:P+2
        else if !tp
          return 0
        else if MsgBox("Error: Malformed inline YAML string: " StrGet(p))
          Exit
      } else if !v&&InStr(", `t",c)&&P+=2
        continue
      else if !v&& (lf.=c,v:=1,P+=2)
        continue
      else if v&& (lf.=c,P+=2)
        continue
      else if MsgBox("Error undefined")
        Exit
    }
    MsgBox("Error: unexpected end of YAML string: " StrGet(p))
    Exit
  }
}