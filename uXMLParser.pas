unit uXMLParser;


{$mode objfpc}{$H+}
{$notes off}

{ On-the-fly XML parser
  Copyright (c) 2012-8, PJ Design Engineering P/L
  All rights reserved.

}

interface
uses Classes;


type
  { Token types returned by parser }
  TTokens = (toElement, toComment, toText);
  { XML tag types }
  TTagTypes = (ttOpening, ttClosing, ttEmpty);
  TStages = (stIdle, stTag, stElement, stName, stValue, stDTD, stNonElement, stInstruction, stParams, stComment, stIgnore, stError);
  TTokenEvent = procedure (Sender : TObject; Token : TTokens; TokenName : string;
                              Tag : TTagTypes; Params : TList) of object;
  TTokenValueEvent = procedure (Sender : TObject; aName, aValue : string) of object;
  TTokenErrorEvent = procedure (Sender: TObject; ErrorMsg : string) of object;

  TParam = class (TObject)
    Name : string;
    Value : string;
    function IntName : integer;
    function IntValue : integer;
    function BoolValue : boolean;
    function CharValue : Char;
    constructor Create;
  end;

  TXMLParser = class (TObject)
  private
    FTagType : TTagTypes;
    FStage : TStages;
    FParam : TParam;
    FText : string;
    FOnToken : TTokenEvent;
    FOnError : TTokenErrorEvent;
    FOnValue : TTokenValueEvent;
  protected
    Params : TList;
    SkipSpaces : boolean;
    Quoted : boolean;
    TextValue : boolean;
    Buffer : TMemoryStream;
    TokenString : string;
    TokenStack : TStringList;
    function GetChar : Char;
    procedure DoParsing;
    procedure ClearParams;
    procedure DoToken (Token : TTokens; var TokenName : string; Tag : TTagTypes; Attributes : TList);
    procedure DoError (ErrorMsg : string);
    procedure DoValue (aName, aValue : string);
  public
    Owner : TObject;
    Tag, Tag2 : integer; // general purpose integers
    constructor Create (anOwner : TObject);
    destructor Destroy; override;
    procedure Parse (Data : string); overload;
    procedure Parse (Data : TStream); overload;
    function Depth : integer;
    procedure Reset;
    property OnToken : TTokenEvent read FOnToken write FOnToken;
    property OnError : TTokenErrorEvent read FOnError write FOnError;
    property OnValue : TTokenValueEvent read FOnValue write FOnValue;
  end;

const
  tgt : array [TTagTypes] of string = ('Opening', 'Closing', 'Empty');
  tkt : array [TTokens] of string = ('Element', 'Comment', 'Text');


implementation
uses SysUtils;
{$REGION 'TParam' }
{ TParam }
function TParam.BoolValue: boolean;
begin
  Result := not ((CharValue = '0') or (CharValue = 'F'));
end;

function TParam.CharValue: Char;
begin
  if Length (Value) = 0 then
    Result := #0
  else
    Result := UpCase (Value[1]);
end;

constructor TParam.Create;
begin
  Name := '';
  Value := '';
end;

function TParam.IntName: integer;
begin
 Result := StrToIntDef (string (Name), -1);
end;

function TParam.IntValue: integer;
begin
  if UpperCase (string (Value)) = 'TRUE' then
    Result := 1
  else if UpperCase (string (Value)) = 'FALSE' then
    Result := 0
  else
    Result := StrToIntDef (string (Value), -1);
end;
{$ENDREGION}
{$REGION 'TXMLParser' }
{ TXMLParser }
procedure TXMLParser.ClearParams;
var
  i : integer;
begin
  for i := 0 to Params.Count - 1 do
    TParam (Params[i]).Free;
  FParam:= nil;
  Params.Clear;
end;

constructor TXMLParser.Create (anOwner: TObject);
begin
  inherited Create;
  Params := TList.Create;
  Owner := anOwner;
  FStage := stIdle;
  SkipSpaces := false;
  FTagType := ttOpening;
  FParam := nil;
  TextValue := false;
  FText := '';
  Buffer := TMemoryStream.Create;
  TokenStack := TStringList.Create;
  FOnToken := nil;
  FOnError := nil;
  FOnValue := nil;
  Tag := 0;
  Tag2 := 0;
end;

function TXMLParser.Depth: integer;
begin
  Result := TokenStack.Count;
end;

destructor TXMLParser.Destroy;
begin
  ClearParams;
  Params.Free;
  TokenStack.Free;
  Buffer.Free;
  inherited;
end;

procedure TXMLParser.DoError (ErrorMsg: string);
begin
  if Assigned (FOnError) then FOnError (Self, ErrorMsg);
end;

procedure TXMLParser.DoParsing;
const
  st : array[TStages] of string =
    ('Idle', 'Tag', 'Element', 'Name', 'Value', 'DTD', 'NonElement', 'Instruction', 'Params', 'Comment', 'Ignore', 'Error');

var
  delims : string;
  aChar : Char;
  CanLoad : boolean;

  function Delim (delims_ : string; chr_ : Char) : boolean;
  begin
    Result := Pos (chr_, delims_) > 0;
  end;

begin
  delims := '<';
  aChar := GetChar;
  while aChar <> #0 do
    begin
      case FStage of
        stIdle   : delims := '<';
        stIgnore : delims := '<>';
        stTag    : delims := '!? '#9'/<>';
        stName   : delims := ' =/<>';
        stValue  : delims := ' '#9'/<>';
        else       delims := '<';
        end;

      CanLoad := (not Delim (delims, aChar)) and (aChar <> #0);
      while CanLoad do
        begin
          if (aChar = #13) and (quoted) then
            begin
              DoError('Unterminated string');
              quoted := false; // error condition
            end;
         case FStage of
            stIdle : TokenString := TokenString + aChar;
            stTag :
              if CharInSet (aChar, ['?', '!']) then
                FStage := stIgnore
              else
                TokenString := TokenString + aChar;
            stName :
              begin
                if FParam = nil then
                  begin
                    FParam := TParam.Create;
                    Params.Add (FParam);
                  end;
                FParam.Name := FParam.Name + aChar;
              end;
            stValue :
              if aChar = '"' then
                begin
                  if quoted then
                    begin
                      CanLoad := false;
                      quoted := false;
                    end
                  else
                    quoted := true;
                  aChar := #0;
                end
              else
                if FParam <> nil then FParam.Value := FParam.Value + aChar;
            end; // case
          if CanLoad then
            begin
              aChar := GetChar;
              CanLoad := (not Delim (delims, aChar)) and (aChar <> #0);
            end;
        end; // while CanLoad

      // delimiters encountered
      if aChar = #0 then
        begin
           // do nothing
        end
      else if (FStage = stValue) and quoted then
        FParam.Value := FParam.Value + aChar
      else if aChar = '<' then
        begin
          ClearParams;
          TokenString := string (Trim (string (TokenString)));
          if length (TokenString) > 0 then DoToken (toText, TokenString, ttEmpty, Params);
          FStage := stTag;
          TokenString := '';
          FParam := nil;
          FTagType := ttOpening;
        end
      else if aChar = '>' then
        begin
          if length (TokenString) > 0 then DoToken (toElement, TokenString, FTagType, Params);
          ClearParams;
          FStage := stIdle;
        end
      else if aChar = '/' then
        begin
          case FStage of
            stTag :
              if length (TokenString) > 0 then
                begin
                  FTagType := ttEmpty;
                  FStage := stIgnore;
                end
              else
                FTagType := ttClosing;
            else
              begin
                FTagType := ttEmpty;
                FStage := stIgnore;
              end;
            end; // case
        end
      else
        begin
          case FStage of
            stIgnore :
              if aChar = '<' then
                begin
                  FStage := stName;
                  TokenString := '';
                  quoted := false;
                  ClearParams;
                  FParam := nil;
                end;
            stName :
              if aChar = '=' then
                begin
                  quoted := false;
                  FStage := stValue;
                end;
            stValue :
              if (aChar = ' ') and quoted then
                FParam.Value := FParam.Value + aChar
              else
                begin
                  FStage := stName;
                  FParam := nil;
                end;
            stTag : FStage := stName;
            end; // case
        end; // else
      aChar := GetChar;
    end;
  Buffer.Clear;
end;

procedure TXMLParser.DoToken (Token: TTokens; var TokenName: string;
  Tag: TTagTypes; Attributes: TList);
begin
  if Token = toElement then
    case Tag of
      ttOpening :
        begin
          FText := '';
          TextValue := false;
          TokenStack.Add (string (TokenName));
        end;
      ttClosing :
        if TokenStack.Count = 0 then
          begin
            DoError ('Orphaned Closing Tag ' + TokenName);
            TextValue := false;
          end
        else
          begin
            if CompareText (TokenStack[TokenStack.Count - 1], string (TokenName)) <> 0 then
              DoError (string ('Unmatched Closing Tag ' + string (TokenName) + ' vs ' + TokenStack[TokenStack.Count - 1]))
            else if TextValue then
              DoValue (TokenName, FText);
            FText := '';
            TokenStack.Delete (TokenStack.Count - 1);
            TextValue := false;
          end;
        ttEmpty : TextValue := false;
      end // case
  else if (Token = toText) and (TokenStack.Count > 0) then
    begin
      FText := TokenName;
      TextValue := true;
    end;
  try
    if Assigned (FOnToken) then
      FOnToken (Self, Token, TokenName, Tag, Attributes);
  except

  end;
  ClearParams;
  TokenName := '';
end;

procedure TXMLParser.DoValue (aName, aValue: string);
begin
  if Assigned (FOnValue) then
    FOnValue (Self, aName, aValue);
end;

function TXMLParser.GetChar: Char;
begin
  Result := #10;  //lf
  while Result = #10 do
    begin
      if Buffer.Position < Buffer.Size then
        Buffer.Read (Result, 1)
      else
        Result := #0;
    end;
end;

procedure TXMLParser.Parse (Data: string);
var
  loc : cardinal;
begin
  loc := Buffer.Position;
  Buffer.Seek (0, soFromEnd);
  Buffer.Write (Data[1], length (Data));
  Buffer.Seek (loc, soFromBeginning);
  DoParsing;
end;

procedure TXMLParser.Parse (Data: TStream);
var
  loc : cardinal;
begin
  loc := Buffer.Position;
  Buffer.Seek (0, soFromEnd);
  Data.Seek (0, soFromBeginning);
  Buffer.CopyFrom (Data, Data.Size);
  Buffer.Seek (loc, soFromBeginning);
  DoParsing;
end;

procedure TXMLParser.Reset;
begin
  Buffer.Clear;
  FStage := stIdle;
  FTagType := ttOpening;
  FParam := nil;
  TokenString := '';
  ClearParams;
end;
{$ENDREGION}
end.
