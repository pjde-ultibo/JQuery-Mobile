program JQMobileDemo;

{$mode objfpc}{$H+}
{$hints off}
{$notes off}

{ Raspberry Pi 3 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  uWebSocket, uFTP, uXMLParser,
  Console, uTFTP, uLog, Winsock2, HTTP,
  Ultibo;

const
  ny : array [boolean] of string = ('NO', 'YES');

type

  { THelper }

  THelper = class
    procedure WSText (aThread : TWSThread; aMsg : TMemoryStream);
    procedure WSPing (aThread : TWSThread; aText : string);
    procedure WSPong (aThread : TWSThread; aText : string);
    procedure WSClose (aThread : TWSThread; aCode : integer; aText : string);
    procedure WSNewClient (aThread : TWSThread);
    procedure WSFinishClient (aThread : TWSThread);
  end;


  { THTTPServer }

  THTTPServer = class (THTTPListener)
    Doc : THTTPDocument;
    Parser : TXMLParser;
    procedure DoToken (Sender : TObject; Token : TTokens; TokenName : string;
                       Tag : TTagTypes; Params : TList);
    function DoGet (aHost: THTTPHost; aDocument: THTTPDocument;
                    aRequest: THTTPServerRequest; aResponse: THTTPServerResponse): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;


var
  Console1, Console2, Console3 : TWindowHandle;
  IPAddress : string;
  aHTTP : THTTPServer;
  aWS : TWSServer;
  aFTP : TFTPserver;
  ch : char;
  aHelper : THelper;
  a : string;
  v : integer;

// General routines
procedure Log1 (s : string);
begin
  ConsoleWindowWriteLn (Console1, s);
end;

procedure Log2 (s : string);
begin
  ConsoleWindowWriteLn (Console2, s);
end;

procedure Log3 (s : string);
begin
  ConsoleWindowWriteLn (Console3, s);
end;

procedure Msg2 (Sender : TObject; s : string);
begin
  Log2 ('TFTP - ' + s);
end;

function WaitForIPComplete : string;
var
  TCP : TWinsock2TCPClient;
begin
  TCP := TWinsock2TCPClient.Create;
  Result := TCP.LocalAddress;
  if (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') then
    begin
      while (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') do
        begin
          sleep (1000);
          Result := TCP.LocalAddress;
        end;
    end;
  TCP.Free;
end;

procedure WaitForSDDrive;
begin
  while not DirectoryExists ('C:\') do sleep (500);
end;

procedure ShowSlider;
begin
  ConsoleWindowSetXY (Console3, 1, 10);
  Log3 ('Slider : ' + v.ToString + '   ');
end;

{ THTTPServer }

procedure THTTPServer.DoToken (Sender: TObject; Token: TTokens;
  TokenName: string; Tag: TTagTypes; Params: TList);
var
  aParam : TParam;
  i : integer;
begin
  if TokenName = 'value' then
    begin
      for i := 0 to Params.Count - 1 do
        begin
          aParam := TParam (Params[i]);
          if aParam.Name = 'value' then
            begin
              v := aParam.IntValue;
              ShowSlider;
            end;
        end;
    end;
end;

function THTTPServer.DoGet (aHost: THTTPHost; aDocument: THTTPDocument;
  aRequest: THTTPServerRequest; aResponse: THTTPServerResponse): Boolean;
var
  s, p : string;
  f : TFileStream;
  x : integer;
  MimeType, ext : string;
begin
  Result := false;
  aResponse.Version := HTTP_VERSION;
  aResponse.Status := HTTP_STATUS_OK;
  aResponse.Reason := HTTP_REASON_200;
  if aRequest.Path = '/' then // base page
    begin
      aResponse.ContentString :=
        '<!DOCTYPE html>'#10 +
        '<html class="ui-mobile">'#10 +
          '<head>'#10 +
            '<title>Slider Test</title>' +
            '<meta name="viewport" content="width=device-width, initial-scale=1"/>'#10 +
            '<link rel="stylesheet" href="jquery.mobile-1.4.5.min.css"/>'#10 +
            '<link rel="stylesheet" href="themes.min.css"/>'#10 +
            '<script src="jquery-1.7.1.min.js"></script>'#10 +
            '<script src="jquery.mobile-1.4.5.min.js"></script>'#10 +
            '<script src="misc.logic.js"></script>'#10 +
          '</head>'#10 +
	        '<body onload="Init (''' + IPAddress + ':' + aWS.Port.ToString + ''')">'#10 +
            '<div data-role="page">'#10 +
              '<form>'#10 +
                '<h1>Slider Test</h1>'#10 +
                '<input id="sdr1" min="0" max="100" value="' + v.ToString + '" type="range" data-highlight="true" data-theme="c"/>'#10 +
              '</form>'#10 +
            '</div>'#10 +
		      '</body>'#10 +
	      '</html>'#10;
    end
  else if aRequest.Path <> '' then
    begin
      p := aRequest.Path;
      for x := 1 to length (p) do if p[x] = '/' then p[x] := '\';
      if p[1] = '\' then p := Copy (p, 2, length (p) - 1);
      s := 'c:\wwwroot\' + p;
      if FileExists (s) then
        begin
          try
            HTTPPathExtractextension (s, ext);
            MimeType := aHost.ResolveMimeType (ext);
            aResponse.SetHeader (HTTP_ENTITY_HEADER_CONTENT_TYPE, MimeType);
            f := TFileStream.Create (s, fmOpenRead or fmShareDenyNone);
            aResponse.Version := HTTP_VERSION;
            aResponse.Status := HTTP_STATUS_OK;
            aResponse.Reason := HTTP_REASON_200;
            aResponse.ContentStream := f;
          except
            aResponse.Version := HTTP_VERSION;
            aResponse.Status := HTTP_STATUS_INTERNAL_SERVER_ERROR;
            aResponse.Reason := HTTP_REASON_500;
            aResponse.ContentString := '<html><head><title>Error ' + HTTPStatusToString (aResponse.Status) + ' (' + aResponse.Reason + ')</title></head><body>Error ' + HTTPStatusToString(AResponse.Status) + ' (' + AResponse.Reason + ')</body></html>' + HTTP_LINE_END;
            end;
        end
      else
        begin
          aResponse.Version := HTTP_VERSION;
          aResponse.Status := HTTP_STATUS_NOT_FOUND;
          aResponse.Reason := HTTP_REASON_404;
          aResponse.ContentString := '<html><head><title>Error ' + HTTPStatusToString (AResponse.Status) + ' (' + AResponse.Reason + ')</title></head><body>Error ' + HTTPStatusToString(AResponse.Status) + ' (' + AResponse.Reason + ')</body></html>' + HTTP_LINE_END;
        end;
    end
  else
    begin
      aResponse.Version := HTTP_VERSION;
      aResponse.Status := HTTP_STATUS_NOT_FOUND;
      aResponse.Reason := HTTP_REASON_404;
      aResponse.ContentString := '<html><head><title>Error ' + HTTPStatusToString (AResponse.Status) + ' (' + AResponse.Reason + ')</title></head><body>Error ' + HTTPStatusToString(AResponse.Status) + ' (' + AResponse.Reason + ')</body></html>' + HTTP_LINE_END;
    end;
  Result := true;
end;

constructor THTTPServer.Create;
begin
  inherited Create;
  Parser := TXMLParser.Create (Self);
  Parser.OnToken := @DoToken;
  Doc := THTTPDocument.Create;
  Doc.OnGet := @DoGet;
  RegisterDocument ('', Doc);
end;

destructor THTTPServer.Destroy;
begin
  Parser.Free;
  DeRegisterDocument ('', Doc);
  Doc.Free;
  inherited Destroy;
end;

{ THelper }

procedure THelper.WSText (aThread: TWSThread; aMsg: TMemoryStream);
var
  bThread : TWSThread;
begin
  aHTTP.Parser.Parse (aMsg);
  bThread := TWSThread (aWS.Threads.First);;
  while bThread <> nil do
    begin
      if aThread <> bThread then bThread.SendString (aMsg);
      bThread := TWSThread (bThread.Next);
    end;
end;

procedure THelper.WSPing (aThread: TWSThread; aText: string);
begin
  Log ('Web Socket PING Received : ' + aText + '.');
end;

procedure THelper.WSPong (aThread: TWSThread; aText: string);
begin
  Log ('Web Socket PONG Received : ' + aText + '.');
end;

procedure THelper.WSClose (aThread: TWSThread; aCode: integer; aText: string);
begin
  Log ('Web Socket CLOSE Received Code : ' + aCode.ToString + ', Reason : ' + aText + '.');
end;

procedure THelper.WSNewClient (aThread: TWSThread);
begin
  Log ('New Web Socket Created.');
end;

procedure THelper.WSFinishClient (aThread: TWSThread);
begin
   Log ('Web Socket Finished With.');
end;

begin
  Console1 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_LEFT, true);
  Console2 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_TOPRIGHT, false);
  Console3 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_BOTTOMRIGHT, false);
  SetLogProc (@Log1);
  Log3 ('JQ Mobile with Web Sockets Test.');
  WaitForSDDrive;
  Log2 ('SD Drive ready.');
  Log2 ('');
  Log3 ('');
  IPAddress := WaitForIPComplete;
  Log3 ('1) Open up a web browser.');
  Log3 ('2) Enter ' + IPAddress + ' in the location bar. You should see a slider.');
  Log3 ('3) Open up another web browser.');
  Log3 ('4) Again enter ' + IPAddress + ' in the location bar.');
  Log3 ('');
  Log3 ('Moving the slider in one broswer should change the position of the slider in the other browser.');
  Log2 ('TFTP - Syntax "tftp -i ' + IPAddress + ' put kernel7.img"');
  SetOnMsg (@Msg2);
  Log2 ('');
  aWS := TWSServer.Create;
  aWS.Port := 8002;
  aHelper := THelper.Create;
  aWS.OnClose := @aHelper.WSClose;
  aWS.OnPing := @aHelper.WSPing;
  aWS.OnPong := @aHelper.WSPong;
  aWS.OnText := @aHelper.WSText;
  aWS.OnNewClient := @aHelper.WSNewClient;
  aWS.OnFinishClient := @aHelper.WSFinishClient;
  aWS.Start;

  aHTTP := THTTPServer.Create;
  aHTTP.BoundPort := 80;
  aHTTP.Active := true;

  v := 34; // initial slider value

  ShowSlider;

  aFTP := TFTPServer.Create;
  // add user accounts and options
  aFTP.AddUser ('admin', 'admin', 'C:\').Options := [foCanAddFolder, foCanChangeFolder, foCanDelete, foCanDeleteFolder, foRebootOnImg];
  aFTP.AddUser ('user', '', 'C:\').Options := [foRebootOnImg];
  aFTP.AddUser ('anonymous', '', 'C:\').Options := [foRebootOnImg];
  // use standard FTP port
  aFTP.BoundPort := 21;
  // set it running
  aFTP.Active := true;

  ch := #0;
  while true do
   begin
     if ConsoleGetKey (ch, nil) then
       case ch of
         'C' : ConsoleWindowClear (Console1);
         'Q', 'q' : break;
         end;
   end;
  Log ('Halted.');
  ThreadHalt (0);
end.

