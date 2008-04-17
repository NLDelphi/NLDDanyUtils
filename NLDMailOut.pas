unit NLDMailOut;

// Dany Rosseel 

{ History of this unit
28-10-2003: * Initial version
20-03-2005: * Converted the "SendMail" procedures into functions. They
              return True (success) or false (failiure)
20-04-2005: * Small correction in "SendMail", made "word" variable an "integer".              
}

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  Classes, IdSMTP;

procedure SetMailOutParams(Host, Port: string; Auth: TAuthenticationType; Id,
  Pw: string);

procedure SetMailOutAddresses(From, Answer: string);

function SendMail(Subject, Towards: string; Body: TStrings;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
overload;

function SendMail(Subject, Towards: string; Body: string;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
overload;

function SendMail(Subject: string; Towards: Tstrings; Body: TStrings;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
overload;

function SendMail(Subject: string; Towards: Tstrings; Body: string;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
overload;


implementation

uses
  SysUtils, IdMessage, NLDSMTPSetup, NLDRcsStrings;

var
  SMTPHost: string;
  SMTPPort: string;
  SMTPAuth: TAuthenticationType;
  SMTPId: string;
  SMTPPw: string;
  SMTPFrom: string;
  SMTPAnswer: string;

  ParamsInitialized: Boolean;
  MailAddressesInitialized: Boolean;

procedure SetMailOutParams(Host, Port: string; Auth: TAuthenticationType; Id,
  Pw: string);
begin
  SMTPHost := Host;
  SMTPPort := Port;
  SMTPAuth := Auth;
  SMTPId := Id;
  SMTPPw := Pw;
  ParamsInitialized := True; // do not reload then any more
end;

procedure SetMailOutAddresses(From, Answer: string);
begin
  SMTPFrom := From;
  SMTPAnswer := Answer;
  MailAddressesInitialized := True; // do not reload them any more
end;

procedure InitParams;
begin
  if not ParamsInitialized then
    GetSmtpValues(SMTPHost, SMTPPort, SMTPAuth, SMTPId, SMTPPw);
end;

procedure InitAddresses;
begin
  if not MailAddressesInitialized then
    GetEmailAddresses(SMTPFrom, SMTPAnswer);
end;

function SendMail(Subject, Towards: string; Body: TStrings;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
var
  Mess: TIdMessage;
  IdSMTP1: TIdSMTP;
  I: Integer;
begin
  Result := false;

  InitParams; { get SMTP settings }
  InitAddresses; { get own addresses }

  IdSMTP1 := TIdSMTP.Create(nil);
  try
    IdSMTP1.host := SMTPHost;
    IdSMTP1.port := strtoint(SMTPPort);
    IdSMTP1.AuthenticationType := SMTPAuth;
    IdSMTP1.Username := SMTPId;
    IdSMTP1.Password := SMTPPw;
    IdSMTP1.MailAgent := 'Microsoft Outlook Express 6.00.2720.3000';
    IdSMTP1.ReadTimeout := 60000;

    Mess := TIdMessage.Create(nil);
    try
      Mess.Encoding := meMIME;
      Mess.AttachmentEncoding := 'MIME';
      Mess.ContentType := 'text/plain';
      Mess.Charset := 'iso-8859-1';
      Mess.ContentTransferEncoding := '7bit';

      Mess.Subject := Subject;

      Mess.Recipients.Emailaddresses := Towards; // comma separated addresses
      Mess.CCList.EMailAddresses := CC; // comma separated addresses
      Mess.BCCList.EMailAddresses := BCC; // comma separated addresses

      Mess.from.Address := SMTPFrom;
      Mess.ReplyTo.Emailaddresses := SMTPAnswer; // comma separated addresses

      Mess.Body.Assign(Body);

      if Assigned(Attachments) then
      begin
        for I := 0 to Attachments.Count - 1 do
        begin
          if FileExists(Attachments[I]) then
            TIdAttachment.Create(Mess.MessageParts, Attachments[I]);
        end;
      end;

      try
        IdSMTP1.Connect;
        try
          IdSMTP1.Send(Mess);
          Result := true; // success
        finally
          IdSMTP1.Disconnect;
        end;
      except
        on Exception do; // failiure
      end;

    finally
      Mess.Free;
    end;

  finally
    IdSMTP1.Free;
  end;
end;

function SendMail(Subject, Towards: string; Body: string;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
var
  Bdy: TStrings;
begin
  Bdy := TStringList.Create;
  try
    Bdy.Add(Body);
    Result := Sendmail(Subject, Towards, Bdy, CC, BCC, Attachments);
  finally
    Bdy.Free;
  end;
end;

function SendMail(Subject: string; Towards: Tstrings; Body: TStrings;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
var
  I: Integer;
begin
  // trim the mail address list
  for I := Towards.Count - 1 downto 0 do
  begin
    Towards[I] := trim(Towards[I]);
    if Towards[I] = '' then Towards.Delete(I);
  end;
  // send the mail
  Result := SendMail(Subject, TStringsToString(Towards), Body, CC, BCC, Attachments);
end;

function SendMail(Subject: string; Towards: Tstrings; Body: string;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
var
  I: Integer;
begin
  // trim the mail address list
  for I := Towards.Count - 1 downto 0 do
  begin
    Towards[I] := trim(Towards[I]);
    if Towards[I] = '' then Towards.Delete(I);
  end;
  // send the mail
  Result := SendMail(Subject, TStringsToString(Towards), Body, CC, BCC, Attachments);
end;

begin
  ParamsInitialized := False;
  MailAddressesInitialized := false;
end.
