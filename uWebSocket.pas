unit uWebSocket;
{$mode objfpc}{$H+}

{
  Copyright (c) 2018, pjde

  JQuery, JQuery Mobile (c) 2010-2012 The JQuery Foundation
  All rights reserved.
}

interface

uses
 Classes, Winsock2, uLog;

const
 // web socket decoding stages
  rsNone              = 0;
  rsHeader            = 1;
  rsExtraLength       = 2;
  rsMask              = 3;
  rsPayload           = 4;
  // web socket op codes
  opContinuation      = $0;
  opText              = $1;
  opBinary            = $2;
  // 3 - 7 reseerved for further non-control frames
  opClose             = $8;
  opPing              = $9;
  opPong              = $a;
  // b - f reserved for further control frames

  WSPort              = 10200;
  ny : array[boolean] of string = ('NO', 'YES');

type
  TWSThread = class;
  TWSServer = class;

  TWSMsgEvent = procedure (aThread : TWSThread; aMsg : TMemoryStream) of object;
  TWSTextEvent = procedure (aThread : TWSThread; aText : string) of object;
  TWSCloseEvent = procedure (aThread : TWSThread; aCode : integer; aText : string) of object;
  TWSClientEvent = procedure (aThread : TWSThread) of object;
  TMsgEvent = procedure (Sender : TObject; s : string) of object;

  { TWSThread }

  TWSThread = class (TWinsock2TCPServerThread)
  private
    FVersion : string;
    FOrigin : string;
    FClientKey, FServerKey : string;
    FProtocol : string;
    FUpgraded : boolean;
    FHandShake : boolean;
    FRxStage : integer;
    FNeed : integer;
    FPayloadLength : integer;
    FOpCode : integer;
    FFinal : Boolean;
    FMasked : Boolean;
    FMask : array [0..3] of byte;
    FRxStream : TMemoryStream;
    FRxPacket : TMemoryStream;
    FOwner : TWSServer;
  public
    ID : string;
    constructor Create (aServer : TWinsock2TCPServer);
    destructor Destroy; override;
    procedure SendString (s : string); overload;
    procedure SendString (s: TMemoryStream); overload;
    function EncodeLength (len : integer) : string;
    procedure SendPing (s : string);
    procedure SendPong (s : string);
    procedure SendClose (Code : integer; aReason : string = '');
  end;

  TWSServer = class (TWinsock2TCPListener)
  private
    FPort : integer;
    FAutoPong : boolean;
    FOnText : TWSMsgEvent;
    FOnPing, FOnPong : TWSTextEvent;
    FOnClose : TWSCloseEvent;
    FOnNewClient, FOnFinishClient : TWSClientEvent;
  protected
    procedure DoCreateThread (aServer : TWinsock2TCPServer; var aThread : TWinsock2TCPServerThread);
    procedure DoConnect (aThread : TWinsock2TCPServerThread); override;
    procedure DoDisconnect (aThread : TWinsock2TCPServerThread); override;
    function DoExecute (aThread : TWinsock2TCPServerThread) : Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    property Port : integer read FPort write FPort;
    property AutoPong : boolean read FAutoPong write FAutoPong;
    property OnText : TWSMsgEvent read FOnText write FOnText;
    property OnPing : TWSTextEvent read FOnPing write FOnPing;
    property OnPong : TWSTextEvent read FOnPong write FOnPong;
    property OnClose : TWSCloseEvent read FOnClose write FOnClose;
    property OnNewClient : TWSClientEvent read FOnNewClient write FOnNewClient;
    property OnFinishClient : TWSClientEvent read FOnFinishClient write FOnFinishClient;
  end;

function GetKey (aKey : string) : string;  // get server key from client's key

implementation

uses
  Crypto, SysUtils;

const
  SpecGUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  CRLF     = #13#10;

function Base64Encode (Input : string) : string;
var
  Final : string;
  Count : Integer;
  Len   : Integer;
const
  Base64Out: array [0..64] of Char =
    ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '=');
begin
  Final := '';
  Count := 1;
  Len := Length (Input);
  while Count <= Len do
    begin
      Final := Final + Base64Out[(Byte (Input[Count]) and $FC) shr 2];
      if (Count + 1) <= Len then
        begin
          Final := Final + Base64Out[((Byte (Input[Count]) and $03) shl 4) +
                                     ((Byte (Input[Count + 1]) and $F0) shr 4)];
          if (Count+2) <= Len then
            begin
              Final := Final + Base64Out[((Byte (Input[Count + 1]) and $0F) shl 2) +
                                         ((Byte (Input[Count + 2]) and $C0) shr 6)];
              Final := Final + Base64Out[(Byte (Input[Count + 2]) and $3F)];
            end
          else
            begin
              Final := Final + Base64Out[(Byte (Input[Count + 1]) and $0F) shl 2];
              Final := Final + '=';
            end
        end
      else
        begin
          Final := Final + Base64Out[(Byte (Input[Count]) and $03) shl 4];
          Final := Final + '==';
        end;
      Count := Count + 3;
    end;
  Result := Final;
end;

function GetKey (aKey : string) : string;  // get server key from client's key
var
  Tmp, Hash : string;
  Context: TSHA1Context;
  Digest: TSHA1Digest;
  i : integer;
begin
  Tmp := aKey + SpecGUID;
  SHA1Init (Context);
  SHA1Update (Context, @Tmp[1], length (Tmp));
  SHA1Final (Context, Digest);
  Hash := '';
  for i := 0 to 19 do Hash := Hash + Char (Digest[i]);
  Result := Base64Encode (Hash);
end;

{ TWSThread - Implements the Web Sockets RFC Protocol }
constructor TWSThread.Create (aServer : TWinsock2TCPServer);
var
  i : integer;
begin
  inherited Create (aServer);
  ID := '';
  FVersion := '';
  FOrigin := '';
  FClientKey := '';
  FServerKey := '';
  FProtocol := '';
  FHandShake := true;
  FUpgraded := false;
  FRxStage := rsNone;
  FMasked := false;
  for i := 0 to 3 do FMask[i] := 0;
  FRxStream := TMemoryStream.Create;
  FRxPacket := TMemoryStream.Create;
end;

destructor TWSThread.Destroy;
begin
  FRxStream.Free;
  FRxPacket.Free;
  inherited;
end;

procedure TWSThread.SendClose (Code : integer; aReason : string);
var
  aRes : string;
begin
  Code := Code mod $10000;
  aRes := AnsiChar ($80 or opClose) + EncodeLength (2 + length (aReason));   // fin + opClose + no mask + payload length
  aRes := aRes + AnsiChar (Code mod $100) + AnsiChar (Code div $100) + aReason;
  Server.WriteData (@aRes[1], length (aRes));
end;

procedure TWSThread.SendPing (s: string);
var
  aRes : AnsiString;
begin
  aRes := AnsiChar ($80 or opPing) + EncodeLength (length (s));
  aRes := aRes + s;
  Server.WriteData (@aRes[1], length (aRes));
end;

procedure TWSThread.SendPong (s: string);
var
  aRes : string;
begin
  aRes := AnsiChar ($80 or opPong) + EncodeLength (length (s));
  aRes := aRes + s;
  Server.WriteData (@aRes[1], length (aRes));
end;

procedure TWSThread.SendString (s: string);
var
  aRes : string;
begin
  aRes := AnsiChar ($80 or opText) + EncodeLength (length (s));
  aRes := aRes + s;
  Server.WriteData (@aRes[1], length (aRes));
end;

procedure TWSThread.SendString (s: TMemoryStream);
var
  aRes : string;
begin
  aRes := AnsiChar ($80 or opText) + EncodeLength (s.Size);
  SetLength (aRes, s.Size + 2);
  Move (s.Memory^, aRes[3], s.Size);
  Server.WriteData (@aRes[1], length (aRes));
end;

function TWSThread.EncodeLength (len : integer): string;
var                            // 64K maximum
  i : integer;
  lenmask, lendiv : uint64;
begin
  if len > $ffff then
    begin
      Result := '';
      lenmask := uint64 ($ff00000000000000);
      lendiv := $100000000000000;
      for i := 7 downto 0 do
        begin
          Result := Result + Char ((len and lenmask) div lendiv);
          lenmask := lenmask div $100;
          lendiv := lendiv div $100;
        end;
    end
  else if len > 125 then // following 2 bytes as length
    begin
      Result := Char ($7e) + Char (len div $100) + Char (len mod $100);
    end
  else
    Result := Char (len); // thats all
end;


{ TWSServer }

        {
procedure TWSServer.ClientDisconnected (Sender: TObject; Client: TWSocketClient;
  Error: Word);
begin
  Mon ('WebSocket Client Disconnected.');
  if Assigned (FOnFinishClient) then FOnFinishClient (Self, TWSClient (Client));
end;     }

constructor TWSServer.Create;
begin
  inherited Create;
  FAutoPong := true;
  BoundPort := WSPort;
  OnCreateThread := @DoCreateThread;
//  Log ('WS Server Created ... ready on ' + LocalAddress);
end;

destructor TWSServer.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TWSServer.DoExecute (aThread: TWinsock2TCPServerThread): Boolean;
var
  aWSThread : TWSThread;
  x, y : integer;
  ba : array [0..1024] of byte;
  b : byte;
  c : integer;
  d, closed : boolean;
  lines : TStringList;
  tmp, tmp2 : string;
  p : int64;
  Response : string;

  procedure DoOutput (opCode : integer);
  var
   s : string;
   y : integer;
  begin
    with aWSThread do
      begin
        if FOpCode in [opPing, opPong, opClose] then
          begin
            FRxPacket.Seek (0, soFromBeginning);
            SetLength (s, FRxPacket.Size);
            FRxPacket.Read (s[1], FRxPacket.Size);
          end
        else
          s := '';
        case FOpCode of
          opText   :
            if Assigned (FOwner.FOnText) then FOwner.FOnText (aWSThread, FRxPacket);
          opBinary :
            begin
              SendClose (1002, 'Binary not supported.'); // check
              Result := false;
            end;
          opPing   :
            begin
              if FOwner.FAutoPong then SendPong (s);
              if Assigned (FOwner.FOnPing) then FOwner.FOnPing (aWSThread, s);
            end;
          opPong   :
            if Assigned (FOwner.FOnPong) then FOwner.FOnPong (aWSThread, s);
          opClose  :
            if length (s) >= 2 then
              begin
                y := ord (s[2]) + $100 * ord (s[1]);
                s := Copy (s, 3, Length (s) - 2);
//                Log ('Close Code ' + y.ToString + ' Reason ' + s);
                if Assigned (FOwner.FOnClose) then FOwner.FOnClose (aWSThread, y, s);
                Result := false;
              end;
          end;
        FRxPacket.Clear;
      end;
  end;

begin
  Result := inherited DoExecute (aThread);
  if not Result then exit;
  aWSThread := TWSThread (aThread);
  c := 1024;
  closed := false;
  d := aThread.Server.ReadAvailable (@ba[0], 1023, c, closed);
  //Log ('c ' + c.ToString + ' closed ' + ny[closed] + ' d ' + ny[d]);
  if closed or not d then Result := false;
  if (c = 0) or closed then exit;
  SetLength (tmp, c);
  Move (ba[0], tmp[1], c);
  //Log ('read ' + inttostr (c));
  with aWSThread do
    begin
      if not FUpgraded then
        begin

          lines := TStringList.Create;
          x := Pos (CRLF, tmp);
          while x > 0 do
            begin
              lines.Add (Copy (tmp, 1, x - 1));
              tmp := Copy (tmp, x + 2, length (tmp) - x);
              x := Pos (CRLF, tmp);
            end;
          if Length (tmp) > 0 then lines.Add (tmp);
          for x := 0 to lines.Count - 1 do
            begin
              y := Pos (': ', lines[x]);
              if y > 0 then
                begin
                  tmp := Copy (lines[x], 1, y - 1);
                  tmp2 := Copy (lines[x], y + 2, length (lines[x]) - y);
                  if tmp = 'Sec-WebSocket-Key' then
                    begin
                      FClientKey := tmp2;
                      FServerKey := GetKey (FClientKey);
                    end
                  else if tmp = 'Sec-WebSocket-Version' then
                    FVersion := tmp2
                  else if tmp = 'Sec-WebSocket-Origin' then
                    FOrigin := tmp2
                  else if tmp = 'Sec-WebSocket-Protocol' then
                    FProtocol := tmp2;
                end;
            end;
          lines.Free;
          if (length (FServerKey) > 0) then
            begin
              Response :=
                'HTTP/1.1 101 Switching Protocols' + CRLF +
                'Upgrade: websocket' + CRLF +
                'Connection: Upgrade' + CRLF +
                'Sec-WebSocket-Accept: ' + FServerKey + CRLF +
                'Sec-WebSocket-Origin: ' + FOrigin + CRLF +
                'Sec-WebSocket-Protocol: ' + FProtocol + CRLF + CRLF;
              Server.WriteData (@Response[1], length (Response));
              FUpgraded := true;
              FRxStage := rsHeader; // expect new massage
              FNeed := 2;       // mimimum needed is 2 bytes
              if Assigned (FOnNewClient) then FOnNewClient (aWSThread);
            end;
        end
      else
        begin
          p := FRxStream.Position;
          FRxStream.Seek (0, soFromEnd);
          FRxStream.Write (tmp[1], length (tmp));
          FRxStream.Seek (p, soFromBeginning);
          while (FRxStream.Size - FRxStream.Position) >= FNeed do
            begin
              case FRxStage of
                rsHeader :
                  begin
                    // need to implement continuation
                    FRxStream.Read (b, 1);
                    FOpCode := b and $0f;
                    FFinal := (b and $80) > 0;
                    // ignore rsvs for now
                    FRxStream.Read (b, 1);
                    FMasked := (b and $80) > 0;
                    FPayLoadLength := b and $7f;
                    if FPayLoadLength = 0 then
                      begin
                        if FMasked then
                          begin
                            FNeed := 4;
                            FRxStage := rsMask;
                          end
                        else
                          begin
                            DoOutput (FOpCode);
                            FNeed := 2;
                            FRxStage := rsHeader;
                          end;
                      end
                    else if FPayloadLength <= 125 then // final length
                      begin
                        if FMasked then
                          begin
                            FNeed := 4;
                            FRxStage := rsMask;
                          end
                        else
                          begin
                            FNeed := FPayLoadLength;
                            FRxStage := rsPayload;
                          end;
                      end
                    else if FPayLoadLength = 126 then
                      begin
                        FRxStage := rsExtraLength;
                        FNeed := 2;
                      end
                    else if FPayLoadLength = 127 then
                      begin
                        FRxStage := rsExtraLength;
                        FNeed := 8;
                      end
                  end;
                rsExtraLength :
                  begin
                    FPayLoadLength := 0;
                    for x := 1 to FNeed do
                      begin
                        FRxStream.Read (b, 1);
                        FPayLoadLength := (FPayLoadLength * $100) + b;
                      end;
                    if FMasked then
                      begin
                        FNeed := 4;
                        FRxStage := rsMask;
                      end
                    else
                      begin
                        FRxStage := rsPayload;
                        FNeed := FPayLoadLength;
                      end;
                  end;
                rsMask :
                  begin
                    FRxStream.Read (FMask, 4);
                    if FPayLoadLength = 0 then
                      begin
                        DoOutput (FOpCode);
                        FRxStage := rsHeader;
                        FNeed := 2;
                      end
                    else
                      begin
                        FRxStage := rsPayload;
                        FNeed := FPayLoadLength;
                      end;
                  end;
                rsPayload :
                  begin
                    SetLength (Tmp, FPayLoadLength);
                    FRxStream.Read (Tmp[1], length (Tmp));
                    if FOpCode <> opContinuation then FRxPacket.Clear;
                    FRxPacket.Seek (0, soFromEnd);
                    for x := 1 to length (Tmp) do
                      begin
                        b := ord (Tmp[x]) xor FMask[(x - 1) mod 4];
                        FRxPacket.Write (b, 1);
                      end;
                    if FFinal then DoOutput (FOpCode);
                    FRxStage := rsHeader;
                    FNeed := 2;
                  end;
                else FRxStream.Read (b, 1);   // avoid infinite loop
              end;    // case
            end;   // while
          if FRxStream.Position = FRxStream.Size then FRxStream.Clear;
        end;
    end;
end;

procedure TWSServer.Start;
begin
  try
    BoundPort := FPort;
    Active := true;
    Log ('Web Sockets Listening.');
  except
    Log ('Web Sockets Listening Error.');
  end;
end;

procedure TWSServer.Stop;
begin
  Active := false;
end;

procedure TWSServer.DoCreateThread (aServer: TWinsock2TCPServer;
  var aThread: TWinsock2TCPServerThread);
begin
//  Log ('Web Socket Thread Created');
  aThread := TWSThread.Create (aServer);
  TWSThread (aThread).FOwner := Self;
end;

procedure TWSServer.DoConnect (aThread: TWinsock2TCPServerThread);
begin
  inherited DoConnect (aThread);
//  Log ('Web Socket Connected');
end;

procedure TWSServer.DoDisconnect (aThread: TWinsock2TCPServerThread);
begin
  inherited DoDisconnect (aThread);
//  Log ('Web Socket Disconnected');
  if Assigned (FOnFinishClient) then FOnFinishClient (TWSThread (aThread));
end;

end.
