unit uFTP;

{$mode delphi}{$H+}
{ $ define ftp_debug}

{ Simple FTP Server
  2017 PJde

  With inspiration from Synapse FTP Server and ICS FTP Server
  and, of cource, Ultibo.

}

interface

uses
  GlobalConfig,
  Platform,
  Threads,
  SysUtils,
  Classes,
  FileSystem,
  Winsock2;

type

  TFTPServer = class;
  TFTPThread = class;
  TFTPPasvListener = class;

  TFTPOptions = (foCanAdd, foCanDelete, foCanAddFolder, foCanChangeFolder, foRebootOnImg, foCanDeleteFolder);
  TFTPOptionSet = set of TFTPOptions;

  { TUserCred }

  TUserCred = class
    User, Pass, Root : string;
    Options : TFTPOptionSet;
    constructor Create;
  end;

  TGetCredEvent = procedure (Sender : TObject; User : string; var Value : string) of object;

  { TFTPPortRxThread }

  TFTPPortRxThread = class (TThread)
    Client : TWinsock2TCPClient;
    RxBuff : TMemoryStream;
    FOnFinished: TNotifyEvent;
    procedure Execute; override;
    constructor Create (aClient : TWinsock2TCPClient);
    destructor Destroy; override;
    property OnFinished : TNotifyEvent read FOnFinished write FOnFinished;
  end;

   { TFTPPasvThread }

  TFTPPasvThread = class (TWinsock2TCPServerThread)
    RxBuff : TMemoryStream;
    constructor Create (aServer : TWinsock2TCPServer);
    destructor Destroy; override;
  end;

  { TFTPThread }

  TFTPThread = class (TWinsock2TCPServerThread)
    Owner : TFTPServer;
    Buff : string;
    Auth : boolean;
    User, Root, Current : string;
    Options : TFTPOptionSet;
    Addr : string;
    Port : integer;
    Sock : TWinsock2TCPClient;
    SockRxThread : TFTPPortRxThread;
    PasvSock : TFTPPasvListener;
    PassiveMode : boolean;
    TypeMode : char;
    FileRename : string;
    FileName : string;
    procedure RxFinished (Sender : TObject);
  public
    function BuildPath (c, s : string) : string;
    function RemoteDir : string;
    function BuildList (s : string) : string;
    function Send (s : string) : boolean;
    function SendRaw (s : string) : boolean; // no crlf
    constructor Create (aServer : TWinsock2TCPServer);
    destructor Destroy; override;
  end;

 { TFTPServer }

  TFTPServer = class (TWinsock2TCPListener)
  private
    FBanner: string;
    FOnGetRoot: TGetCredEvent;
    FOnGetPass : TGetCredEvent;
  protected
    procedure DoCreateThread (aServer : TWinsock2TCPServer; var aThread : TWinsock2TCPServerThread);
    procedure DoConnect (aThread : TWinsock2TCPServerThread); override;
    procedure DoDisconnect (aThread : TWinsock2TCPServerThread); override;
    function DoExecute (aThread : TWinsock2TCPServerThread) : Boolean; override;
  public
    Users : TList;
    PasvPort : Word;
    constructor Create;
    destructor Destroy; override;
    function GetNextPasvPort : Word;
    function AddUser (User, Pass, Root : string) : TUserCred;
    procedure ClearUsers;
    function GetCreds (User : string) : TUserCred;
    function DoGetPass (User : string) : string;
    property Banner : string read FBanner write FBanner;
    property OnGetRoot : TGetCredEvent read FOnGetRoot write FOnGetRoot;
    property OnGetPass : TGetCredEvent read FOnGetPass write FOnGetPass;
  end;

  { TFTPPasvListener }

  TFTPPasvListener = class (TWinsock2TCPListener)
    Owner : TFTPThread;
  protected
    procedure DoCreateThread (aServer : TWinsock2TCPServer; var aThread : TWinsock2TCPServerThread);
    procedure DoConnect (aThread : TWinsock2TCPServerThread); override;
    procedure DoDisconnect (aThread : TWinsock2TCPServerThread); override;
    function DoExecute (aThread : TWinsock2TCPServerThread) : Boolean; override;
  public
    Port : Word;
    TxBuff : TMemoryStream;
    constructor Create (anOwner : TFTPThread);
    destructor Destroy; override;
  end;


var
  TFTP : TFTPServer = nil;

implementation

uses uLog;

const
  MonthNames : array[1..12] of AnsiString =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

  FirstPasvPort = 10001;
  LastPasvPort  = 11000;

//  ny : array [boolean] of string = ('NO','YES');

procedure DoLog (s : string);
begin
  if s = '' then begin end;
{$ifdef ftp_debug}
  Log (s);
{$endif}
end;

function display_string (s : string; show : boolean) : string;
var
  i : integer;
begin
  Result := '';
  for i := 1 to length (s) do
    if s[i] in [' '..'~'] then
      Result := Result + s[i]
    else if show then
      Result := Result + '<' + IntToStr (ord (s[i])) + '>';
end;

function ParseAddr (s : string; var Addr : string; var Port : integer) : boolean;
var
  res : TStringList;
  st, en, x : integer;
begin
  Result := false;
  Addr := '';
  Port := 0;
  st := Pos ('(', s);
  en := Pos (')', s);
  if (en > st) and (en > 1) then
    begin
      s := Copy (s, st + 1, en - st - 1);
    end;
  res := TStringList.Create;
  x := Pos (',', s);
  while x > 0 do
    begin
      res.Add (Copy (s, 1, x - 1));
      s := Copy (s, x + 1, length (s) - x);
      x := Pos (',', s);
    end;
  res.Add (s);
  if res.Count = 6 then
    begin
      Result := true;
      for x := 0 to res.Count - 1 do if not (StrToIntDef (res[x], - 1) in [0..255]) then Result := false;
    end;
  if Result then
    begin
      Addr := res[0] + '.' + res[1] + '.' + res[2] + '.' + res[3];
      Port := StrToInt (res[4]) * 256 + StrToInt (res[5]);
    end;
  res.Free;
end;

function FDate (value: integer) : string;
var
  st : tdatetime;
  wYear, wMonth, wDay : Word;
  wHour, wMinute, wSecond, wMiliSecond : Word;
begin
  st := FileDateToDateTime (value);
  DecodeDate (st, wYear, wMonth, wDay);
  DecodeTime (st, wHour, wMinute, wSecond, wMiliSecond);
  Result:= Format ('%s%3d %0.4d', [MonthNames[wMonth], wDay, wYear]);
end;

{ TFTPPortRxThread }

procedure TFTPPortRxThread.Execute;
var
  x : int64;
  b : array [0..255] of byte;
  c : integer;
  closed, d : boolean;
begin
  while not Terminated do
    begin
      closed := false;
      c := 0;
      d := Client.ReadAvailable (@b[0], 255, c, closed);
      if closed or not d then break;
      x := RxBuff.Position;     // add new data to end of stream
      RxBuff.Seek (0, soFromEnd);
      RxBuff.Write (b[0], c);
      RxBuff.Seek (x, soFromBeginning);  // goto back to where we are
    end;
  if Assigned (FOnFinished) then FOnFinished (Self);
end;

constructor TFTPPortRxThread.Create (aClient: TWinsock2TCPClient);
begin
  inherited Create (true);
  Client := aClient;
  RxBuff := TMemoryStream.Create;
  FreeOnTerminate := true;
end;

destructor TFTPPortRxThread.Destroy;
begin
  RxBuff.Free;
  inherited Destroy;
end;

{ TFTPPasvThread }

constructor TFTPPasvThread.Create (aServer: TWinsock2TCPServer);
begin
  inherited Create (aServer);
  RxBuff := TMemoryStream.Create;
end;

destructor TFTPPasvThread.Destroy;
begin
   RxBuff.Free;
  inherited Destroy;
end;

{ TUserCred }

constructor TUserCred.Create;
begin
  User := '';
  Pass := '';
  Root := '';
  Options := [];
end;

{ TFTPPasvListener }

procedure TFTPPasvListener.DoCreateThread (aServer: TWinsock2TCPServer;
  var aThread: TWinsock2TCPServerThread);
begin
  aThread := TFTPPasvThread.Create (aServer);
end;

procedure TFTPPasvListener.DoConnect (aThread: TWinsock2TCPServerThread);
begin
  inherited DoConnect (aThread);
  TFTPPasvThread (aThread).RxBuff.Clear;
  if TxBuff.Size > 0 then
    begin
      aThread.Server.WriteData (TxBuff.Memory, TxBuff.Size);
      aThread.Server.Shutdown;
      TxBuff.Clear;
    end;
end;

procedure TFTPPasvListener.DoDisconnect (aThread: TWinsock2TCPServerThread);
var
  f : TFileStream;
  s : string;
begin
  inherited DoDisconnect (aThread);
  if Owner.PassiveMode then
    begin
      if (TFTPPasvThread (aThread).RxBuff.Size > 0) and (Owner.FileName <> '') then
        begin
          try
            f := TFileStream.Create (Owner.FileName, fmCreate);
            f.CopyFrom (TFTPPasvThread (aThread).RxBuff, 0);
            f.Free;
            TFTPPasvThread (aThread).RxBuff.Clear;
            Owner.Send ('226 OK');
            if foRebootOnImg in Owner.Options then
              begin
                s := UpperCase (ExtractFileName (Owner.FileName));
                if (s = 'KERNEL.IMG') or (s = 'KERNEL7.IMG') then
                  SystemRestart (0);
              end;
          except
            end;
        end
      else
        Owner.Send ('226 OK');
    end;
  TxBuff.Clear;
  Active := false;
end;

function TFTPPasvListener.DoExecute (aThread: TWinsock2TCPServerThread): Boolean;
var
  aPasvThread : TFTPPasvThread;
  x : int64;
  b : array [0..255] of byte;
  c : integer;
  closed, d : boolean;
begin
  Result := inherited DoExecute (aThread);
  if not Result then exit;
  aPasvThread := TFTPPasvThread (aThread);
  c := 256;
  closed := false;
  d := aPasvThread.Server.ReadAvailable (@b[0], 255, c, closed);
  if closed or not d then Result := false;
  if not Result then exit;
  with aPasvThread do
    begin
      x := RxBuff.Position;     // add new data to end of stream
      RxBuff.Seek (0, soFromEnd);
      RxBuff.Write (b[0], c);
      RxBuff.Seek (x, soFromBeginning);  // goto back to where we are
    end;
end;

constructor TFTPPasvListener.Create (anOwner : TFTPThread);
begin
  inherited Create;
  Owner := anOwner;
  OnCreateThread := DoCreateThread;
  TxBuff := TMemoryStream.Create;
end;

destructor TFTPPasvListener.Destroy;
begin
  TxBuff.Free;
  inherited Destroy;
end;

{ TFTPThread }

function TFTPThread.BuildList (s : string) : string;
var
  sr : TSearchRec;
  err : integer;
  res : string;
begin
  if s = '' then s := '';
  result := '';
  if Root = '' then exit;
  err := FindFirst (BuildPath (Current, '*.*'), faAnyFile, sr);
  while err = 0 do
    begin
      if ((sr.Attr and faHidden) = 0)
        and ((sr.Attr and faSysFile) = 0)
        and ((sr.Attr and faVolumeID) = 0) then
        begin
          res := '';
          if (sr.Attr and faDirectory) > 0 then
            begin        // add .. if not at root
              if (sr.Name <> '.') and (sr.Name <> '..') and (foCanChangeFolder in Options) then
                begin
                  res := res + 'drwxrwxrwx 1 root root ';
                  res := res + format ('%13d', [sr.Size]) + ' ';
                  res := res + FDate (sr.Time) + ' ';
                  res := res + sr.Name;
                end;
            end
          else
            begin
              res := res + '-rwxrwxrwx 1 root root ';
              res := res + format ('%13d', [sr.Size]) + ' ';
              res := res + FDate (sr.Time) + ' ';
              res := res + sr.Name;
            end;
          if res <> '' then Result := Result + res + #13#10;
        end;
      err := FindNext (sr);
    end;
  FindClose (sr);
end;

procedure TFTPThread.RxFinished (Sender: TObject);
var
  f : TFileStream;
  s : string;
begin
  if not PassiveMode then
    begin
      if FileName <> '' then
        begin
          try
            f := TFileStream.Create (FileName, fmCreate);
            f.CopyFrom (TFTPPortRxThread (Sender).RxBuff, 0);
            f.Free;
            TFTPPortRxThread (Sender).RxBuff.Clear;
            Send ('226 OK');
            if foRebootOnImg in Options then
              begin
                s := UpperCase (ExtractFileName (FileName));
                if (s = 'KERNEL.IMG') or (s = 'KERNEL7.IMG') then
                  begin
                    SystemRestart (0);
                  end;
              end;
          except
            on e : Exception do
              Send ('550 ' + e.Message);
          end;
        end;
      FileName := '';
    end;
end;

function TFTPThread.BuildPath (c, s: string): string;
begin
  Result := '';
  if Root = '' then exit;
  Result := Root;
  if c = '' then
    begin
      if Root[length (Root)] <> '\' then Result := Result + '\';
      Result := Result + s;
    end
  else
    begin
      if (Result[length (Result)] <> '\') then Result := Result + '\';
      if (c[1] = '\') then
        Result := Result + Copy (c, 2, length (c) - 1)
      else
        Result := Result + c;
      if (Result[length (Result)] <> '\') then Result := Result + '\';
      Result := result + s;
    end;
end;

function TFTPThread.RemoteDir: string;
var
  i : integer;
begin
  if Current = '' then
    Result := '/'
  else
    begin
      Result := Current;
      if Result[length (Result)] = '\' then Result := Copy (Result, 1, length (Result) - 1);
      for i := 1 to length (Result) do
        if Result[i] = '\' then Result[i] := '/';
    end;
end;

function TFTPThread.Send (s: string) : boolean;
begin
  Result := SendRaw (s + #13#10);
end;

function TFTPThread.SendRaw (s : string) : boolean; // no crlf
begin
  DoLog ('< ' + display_string (s, false));
  Result := Server.WriteData (@s[1], length (s));
end;

constructor TFTPThread.Create (aServer: TWinsock2TCPServer);
begin
  inherited Create (aServer);
  Buff := '';
  User := '';
  Root := '';
  Current := '';
  Auth := false;
  Addr := '';
  Port := 0;
  FileName := '';
  FileRename := '';
  PassiveMode := false;
  TypeMode := 'I';
  Sock := TWinsock2TCPClient.Create;
  SockRxThread := nil;
  PasvSock := TFTPPasvListener.Create (Self);
end;

destructor TFTPThread.Destroy;
begin
  Sock.Free;
  if Assigned (SockRxThread) then SockRxThread.Terminate;
  PasvSock.Free;
  inherited Destroy;
end;

{ TFTPServer }

procedure TFTPServer.DoCreateThread (aServer: TWinsock2TCPServer;
  var aThread: TWinsock2TCPServerThread);
begin
  aThread := TFTPThread.Create (aServer);
  TFTPThread (aThread).Owner := Self;
end;

procedure TFTPServer.DoConnect (aThread: TWinsock2TCPServerThread);
begin
  inherited DoConnect (aThread);
  TFTPThread (aThread).Send ('220 ' + FBanner);
end;

procedure TFTPServer.DoDisconnect (aThread: TWinsock2TCPServerThread);
begin
  inherited DoDisconnect (aThread);
end;

function TFTPServer.DoExecute (aThread: TWinsock2TCPServerThread): Boolean;
var
  line, cmd, param, s, p : string;
  c : integer;
  x, y : integer;
  sr : TSearchRec;
  err : integer;
  closed, d : boolean;
  b : array [0..255] of byte;
  uc : TUserCred;
  f : TFileStream;
  m : TMemoryStream;
  anFTPThread : TFTPThread;
  aSockThread : TWinsock2SocketThread;
begin
  Result := inherited DoExecute (aThread);
  if not Result then exit;
  anFTPThread := TFTPThread (aThread);
  c := 256;
  closed := false;
  d := aThread.Server.ReadAvailable (@b[0], 255, c, closed);
  if closed or not d then Result := false;
  //if (c = 0) or closed then exit;
  if not Result then exit;
  with anFTPThread do
    begin
      x := length (Buff);
      SetLength (Buff, Length (Buff) + c);
      Move (b[0], Buff[x + 1], c);
      x := Pos (#13#10, Buff);
      while x > 0 do
        begin
          Line := Trim (Copy (Buff, 1, x - 1));
          Buff := Copy (Buff, x + 2, length (Buff) - x);
          y := Pos (' ', Line);
          if y > 1 then
             begin
              cmd := Copy (Line, 1, y - 1);
              param := Copy (Line, y + 1, length (Line) - y);
            end
          else
            begin
              cmd := Line;
              param := '';
            end;
          cmd := UpperCase (cmd);
          DoLog ('> ' + cmd + ' ' + param);
          if cmd = '' then
            begin
            end
          else if cmd = 'USER' then
            begin
              User := param;
              s := DoGetPass (User);
              if length (s) > 0 then
                 Send ('331 Please specify the password.')
              else
                begin
                  uc := GetCreds (User);
                  if uc <> nil then
                    begin
                      Options := uc.Options;
                      Root := uc.Root;
                    end;
                  Current := '';
                  auth := true;
                  Send ('230 Login successful.');
                end;
            end
          else if cmd = 'PASS' then
            begin
              s := DoGetPass (User);
              if User = '' then
                Send ('550 Username Required.')
              else if s = '' then
                Send ('230 Login successful.') // already logged in
              else if param = s then
                begin
                  uc := GetCreds (User);
                  if uc <> nil then
                    begin
                      Options := uc.Options;
                      Root := uc.Root;
                    end;
                  Current := '';
                  auth := true;
                  Send ('230 Login successful.');
                end
              else
                Send ('550 Unknown User Credentials.');
            end
          else if cmd = 'QUIT' then
            begin
              Send ('221 Goodbye.');
              Result := false;
              break;
            end
          else if not auth then
            Send ('550 Access Denied.')
          else if cmd = 'NOOP' then
            Send ('200 OK')
          else if cmd = 'LANG' then // any language you like if you want english
            Send ('200 OK')
          else if cmd = 'PWD' then
            begin
              Send (format ('257 "%s" is current directory.', [RemoteDir]));
            end
          else if cmd = 'SYST' then
            Send ('215 UNIX Type: L8')
          else if cmd = 'SIZE' then
            begin
              err := FindFirst (BuildPath (Current, param), faAnyFile, sr);
              if err = 0 then
                Send ('213 ' + IntToStr (sr.Size))
              else
                Send ('450 File not found.');
              FindClose (sr);
            end
          else if cmd = 'PORT' then
            begin
              PassiveMode := false;
              PasvSock.Active := false;
              if ParseAddr (param, Addr, Port) then
                 Send ('200 OK')
              else
                 Send ('550 Not logged in.');
            end
          else if cmd = 'OPTS' then  // will try and turn utf on
            Send ('200 OK')
          else if cmd = 'RETR' then
            begin
              s := BuildPath (Current, param);
              if s = '' then
                Send ('550 Not authorised.')
              else if not FileExists (s) then
                Send ('550 File unavailable.')
              else if PassiveMode then
                begin
                  Send ('150 OK');
                  PasvSock.TxBuff.Clear;
                  try
                    f := TFileStream.Create (s, fmOpenRead or fmShareDenyWrite);
                    f.Seek (0, soFromBeginning);
                    PasvSock.TxBuff.CopyFrom (f, 0);
                    f.Free;

                    if PasvSock.Threads.Count > 0 then  // connected already
                      begin
                        aSockThread := PasvSock.Threads.First;
                        if aSockThread <> nil then
                          begin
                            TWinsock2TCPServerThread (aSockThread).Server.WriteData (PasvSock.TxBuff.Memory, PasvSock.TxBuff.Size);
                            TWinsock2TCPServerThread (aSockThread).Server.Shutdown;
                            PasvSock.TxBuff.Clear;
                          end;
                      end;
                  except
                    on e:Exception do
                      Send ('451 ' + e.Message + '.');
                    end;
                end
              else       // non passive
                begin
                  try
                    Sock.CloseSocket;
                  except
                    end;
                  try
                    Sock.RemoteAddress := Addr;
                    Sock.RemotePort := Port;
                    Sock.Connect;
                  except
                    end;
                  if not Sock.Connected then
                    Send ('425 Can''t open data connection.')
                  else
                    begin
                      Send ('150 OK');
                      m := TMemoryStream.Create;
                      try
                        f := TFileStream.Create (s, fmOpenRead or fmShareDenyWrite);
                        f.Seek (0, soFromBeginning);
                        m.CopyFrom (f, 0);
                        f.Free;
                        Sock.WriteData (m.Memory, m.Size);
                        Sock.Shutdown;
                        Send ('226 OK');
                      except
                        on E:Exception do
                          Send ('451 ' + e.Message + '.');
                        end;
                      m.Free;
                    end;
                end;  // non passive
            end
          else if cmd = 'STOR' then
            begin
              s := BuildPath (Current, param);
              if not DirectoryExists (ExtractFiledir (s)) then
                Send ('550 Invalid path.')
              else if PassiveMode then
                begin
                  FileName := s;
                  aSockThread := PasvSock.Threads.First;
                  if aSockThread <> nil then TFTPPasvThread (aSockThread).RxBuff.Clear;
                  Send ('150 OK');
                end
              else       // non passive
                begin
                  FileName := s;
                  try
                    Sock.CloseSocket;
                  except
                    end;
                  try
                    Sock.RemoteAddress := Addr;
                    Sock.RemotePort := Port;
                    Sock.Connect;
                  except
                    end;
                  if not Sock.Connected then
                    Send ('425 Can''t open data connection.')
                  else
                    begin     // we could probably block receive the data here without need of thread
                      SockRxThread := TFTPPortRxThread.Create (Sock);
                      SockRxThread.OnFinished := RxFinished;
                      SockRxThread.Start;
                      Send ('150 OK');
                    end;
                end;  // non passive
            end
          else if (cmd = 'LIST') or (cmd = 'NLST') then
            begin
              if PassiveMode then
                begin
                  Send ('150 OK');
                  s := BuildList (param);
                  PasvSock.TxBuff.Clear;
                  PasvSock.TxBuff.Write (s[1], length (s));
                  if PasvSock.Threads.Count > 0 then  // connected already
                    begin
                      aSockThread := PasvSock.Threads.First;
                      if aSockThread <> nil then
                        begin
                          TWinsock2TCPServerThread (aSockThread).Server.WriteData (PasvSock.TxBuff.Memory, PasvSock.TxBuff.Size);
                          TWinsock2TCPServerThread (aSockThread).Server.Shutdown;
                          PasvSock.TxBuff.Clear;
                        end;
                    end;
                end
              else  // active mode
                begin
                  try
                    Sock.CloseSocket;
                  except
                    end;
                  try
                    Sock.RemoteAddress := Addr;
                    Sock.RemotePort := Port;
                    Sock.Connect;
                  except
                    end;
                  if not Sock.Connected then
                    Send ('425 Can''t open data connection.')
                  else
                    begin
                      Send ('150 OK');
                      s := BuildList (param);
                      Sock.WriteData (@s[1], length (s));
                      Send ('226 OK');
                      Sock.Shutdown;
                    end;
                end;
            end
          else if cmd = 'PASV' then
            begin
              PassiveMode := true;
              PasvSock.Active := false;
              Addr := Sock.LocalAddress;
              Port := GetNextPasvPort;
              PasvSock.BoundPort := Port;
              PasvSock.Active := true;
              s := Addr;
              for x := 1 to length (s) do if s[x] = '.' then s[x] := ',';
              Send (format ('227 Entering Passive Mode (%s,%d,%d).', [s, Port div $100, Port mod $100]));
            end
          else if cmd = 'TYPE' then
            begin
              if length (param) = 1 then TypeMode := param[1];
              Send ('200 OK');
            end
          else if cmd = 'DELE' then
            begin
              if not (foCanDelete in Options) then
                Send ('550 Not authorised.')
              else
                begin
                  s := BuildPath (Current, param);
                  if s = '' then
                    Send ('550 Access denied.')
                  else if not FileExists (s) then
                    Send ('550 File not found.')
                  else
                    begin
                      DeleteFile (s);
                      Send ('250 OK');
                    end;
                end;
            end
          else if (cmd = 'MKD') or (cmd = 'XMKD') then
            begin
              if not (foCanAddFolder in Options) then
                Send ('550 Access denied.')
              else
                begin
                  s := BuildPath (Current, '');
                  //Log ('Base directory ' + s);
                  if DirectoryExists (s) then
                    begin
                      //Log ('Base directory exists');
                      s := BuildPath (Current, param);
                      //Log ('Proposed directory ' + s);
                      if CreateDir (s) then
                        Send (format ('257 "%s" directory created.', [param]))
                      else
                        Send ('550 Directory creation failed.');
                    end
                  else
                   Send ('550 Directory creation failed.');
                end;
            end
          else if cmd = 'RNFR' then
            begin
              s := BuildPath (Current, param);
              err := FindFirst (s, faAnyFile, sr);
              if err = 0 then
                begin
                  Send ('350 OK');
                  FileRename := s;
                end
              else
                begin
                  Send ('450 File not found.');
                  FileRename := '';
                end;
              FindClose (sr);
            end
          else if cmd = 'RNTO' then
            begin
              //Log ('File to rename ' + FileRename);
              s := ExtractFilePath (FileRename) + param;
              //Log ('New filename ' + s);
              if FileRename = '' then
                Send ('503 Bad sequence of commands.')
              else if RenameFile (FileRename, s) then
                Send ('250 OK')
              else
                Send ('550 File was not renamed.');
            end
          else if cmd = 'RMD' then
            begin
              if not (foCanDeleteFolder in Options) then
                Send ('550 Access Denied.')
              else
                begin
                  s := BuildPath (Current, param);
                  err := FindFirst (s, faAnyFile, sr);
                  if (err = 0) and ((sr.Attr and faDirectory) > 0) then
                    begin
                      if RemoveDir (s) then
                        Send ('250 OK')
                      else
                        Send ('550 Directory not deleted.');
                    end;
                  FindClose (sr);
                end;
            end
          else if (cmd = 'CWD') or (cmd = 'XCMD') then
            begin
              for x := 1 to length (param) do if Param[x] = '/' then Param[x] := '\';
              if Param = '' then
                s := Current
              else if Param = '\' then
                s := ''     // root directory
              else if Param = '..' then // back a directory
                begin
                  s := Current;
                  x := length (s);
                  while (x > 0) do
                    begin
                      if s[x] = '\' then
                        begin
                          Delete (s, x, 1);
                          break;
                        end
                      else
                        Delete (s, x, 1);
                      x := x - 1;
                    end;
                end
              else if Param[1] = '\' then
                s := Copy (Param, 2, Length (Param) - 1)
              else if Current = '' then
                s := Param
              else if Current[length (Current)] = '\' then
                s := Current + Param
              else
                s := Current + '\' + Param;
              p := BuildPath (s, '');
              //Log ('p : ' + p + ' s : ' + s + ' param : ' + param);
              if (s <> '') and (not (foCanChangeFolder in Options)) then
                Send ('550 Access Denied.')
              else if DirectoryExists (p) then
                begin
                  Current := s;
           //       Send (format ('250 "%s" is current directory.', [RemoteDir]));
                  Send ('250 OK');
                end
              else
                Send ('550 No such directory.');
            end
          else
            Send ('550 Unrecognised command.');
          x := Pos (#13#10, Buff);
        end;
    end;
end;

constructor TFTPServer.Create;
begin
  inherited Create;
  BoundPort := 21;
  PasvPort := FirstPasvPort - 1;
  Users := TList.Create;
  FBanner := 'Welcome to Ultibo FTP Server';
  OnCreateThread := DoCreateThread;
end;

destructor TFTPServer.Destroy;
begin
  ClearUsers;
  Users.Free;
  inherited Destroy;
end;

function TFTPServer.GetNextPasvPort: Word;
begin
  PasvPort := PasvPort + 1;
  if PasvPort > LastPasvPort then PasvPort := FirstPasvPort;
  Result := PasvPort;
end;

function TFTPServer.AddUser (User, Pass, Root: string) : TUserCred;
begin
  Result := TUserCred.Create;
  Result.User := user;
  Result.Pass := pass;
  Result.Root := root;
  Users.Add (Result);
end;

procedure TFTPServer.ClearUsers;
var
  i : integer;
begin
  for i := 0 to Users.Count - 1 do TUserCred (Users[i]).Free;
  Users.Clear;
end;

function TFTPServer.GetCreds (User: string): TUserCred;
var
  i : integer;
begin
  for i := 0 to Users.Count - 1 do
    begin
      Result := Users[i];
      if (Result.User = User) then exit;
    end;
  Result := nil;
end;

function TFTPServer.DoGetPass (User: string): string;
var
  uc : TUserCred;
  i : integer;
begin
  if Assigned (FOnGetPass) then
    begin
      FOnGetPass (Self, User, Result);
   end
  else
    begin
      Result := '';
      for i := 0 to Users.Count - 1 do
        begin
          uc := Users[i];
          if (uc.User = User) then
            begin
              Result := uc.Pass;
              exit;
            end;
        end;
    end;
end;


initialization


(*  List of all known return codes that may be issued by an FTP server.

Code 	Explanation
100 Series 	The requested action is being initiated, expect another reply before proceeding with a new command.
110 	Restart marker replay . In this case, the text is exact and not left to the particular implementation; it must read: MARK yyyy = mmmm where yyyy is User-process data stream marker, and mmmm server's equivalent marker (note the spaces between markers and "=").
120 	Service ready in nnn minutes.
125 	Data connection already open; transfer starting.
150 	File status okay; about to open data connection.
200 Series 	The requested action has been successfully completed.
202 	Command not implemented, superfluous at this site.
211 	System status, or system help reply.
212 	Directory status.
213 	File status.
214 	Help message. Explains how to use the server or the meaning of a particular non-standard command. This reply is useful only to the human user.
215 	NAME system type. Where NAME is an official system name from the registry kept by IANA.
220 	Service ready for new user.
221 	Service closing control connection.
225 	Data connection open; no transfer in progress.
226 	Closing data connection. Requested file action successful (for example, file transfer or file abort).
227 	Entering Passive Mode (h1,h2,h3,h4,p1,p2).
228 	Entering Long Passive Mode (long address, port).
229 	Entering Extended Passive Mode (|||port|).
230 	User logged in, proceed. Logged out if appropriate.
231 	User logged out; service terminated.
232 	Logout command noted, will complete when transfer done.
234 	Specifies that the server accepts the authentication mechanism specified by the client, and the exchange of security data is complete. A higher level nonstandard code created by Microsoft.
250 	Requested file action okay, completed.
257 	"PATHNAME" created.
300 Series 	The command has been accepted, but the requested action is on hold, pending receipt of further information.
331 	User name okay, need password.
332 	Need account for login.
350 	Requested file action pending further information
400 Series 	The command was not accepted and the requested action did not take place, but the error condition is temporary and the action may be requested again.
421 	Service not available, closing control connection. This may be a reply to any command if the service knows it must shut down.
425 	Can't open data connection.
426 	Connection closed; transfer aborted.
430 	Invalid username or password
434 	Requested host unavailable.
450 	Requested file action not taken.
451 	Requested action aborted. Local error in processing.
452 	Requested action not taken. Insufficient storage space in system.File unavailable (e.g., file busy).
500 Series 	Syntax error, command unrecognized and the requested action did not take place. This may include errors such as command line too long.
501 	Syntax error in parameters or arguments.
502 	Command not implemented.
503 	Bad sequence of commands.
504 	Command not implemented for that parameter.
530 	Not logged in.
532 	Need account for storing files.
534 	Could Not Connect to Server - Policy Requires SSL
550 	Requested action not taken. File unavailable (e.g., file not found, no access).
551 	Requested action aborted. Page type unknown.
552 	Requested file action aborted. Exceeded storage allocation (for current directory or dataset).
553 	Requested action not taken. File name not allowed.
600 Series 	Replies regarding confidentiality and integrity
631 	Integrity protected reply.
632 	Confidentiality and integrity protected reply.
633 	Confidentiality protected reply.
10000 Series 	Common Winsock Error Codes
10054 	Connection reset by peer. The connection was forcibly closed by the remote host.
10060 	Cannot connect to remote server.
10061 	Cannot connect to remote server. The connection is actively refused by the server.
10066 	Directory not empty.
10068 	Too many users, server is full. *)

end.

