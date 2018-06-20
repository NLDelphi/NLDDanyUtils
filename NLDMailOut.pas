unit NLDMailOut;

// Dany Rosseel

{ History of this unit
28-10-2003: * Initial version
20-03-2005: * Converted the "SendMail" procedures into functions. They
              return True (success) or false (failiure)
20-04-2005: * Small correction in "SendMail", made "word" variable an "integer".
19-06-2018: * Simplified the implementation, does not uses the 'SMTPSetup' unit any more,
              instead the unit 'SMTP_Register' has been added (common for this unit ans 'SMTPSetup').
            * The routine 'Recall_Default_MailOut_Params_and_Addresses' is added.
}

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  Classes, IdSMTP;

procedure SetMailOutParams(Host, Port: string; Auth: TAuthenticationType; Id,
  Pw: string);

procedure SetMailOutAddresses(From, Answer: string);

procedure Recall_Default_MailOut_Params_and_Addresses;

function SendMail(Subject, Towards: string; Body: TStrings;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
overload;

function SendMail(Subject, Towards, Body: string;
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
  SysUtils, IdMessage, NLDRcsStrings, NLDSimpleRegistry, SMTP_Register;

var
  Info: record
    SmtpServer: string;
    SmtpPort: string;
    SmtpUseLogin: TAuthenticationType;
    SmtpAccountName: string;
    SmtpPassword: string;
    EmailAddress: string;
    ReplyAddress: string;
  end;

procedure ReadSmtp;
var
  F: TSimpleRegistry;
begin
  F := TSimpleRegistry.Create;
  Info.SmtpServer := F.ReadString(SmtpRegister + 'Configuration', 'Server', '');
  Info.SmtpPort := F.ReadString(SmtpRegister + 'Configuration', 'Port', '25');
  Info.SmtpUseLogin := TAuthenticationType(F.ReadInteger(SmtpRegister + 'Login',
    'UseLogin', Integer(atNone)));
  Info.SmtpAccountName := F.ReadString(SmtpRegister + 'Login', 'AccountName',
    '');
  Info.SmtpPassword := F.ReadString(SmtpRegister + 'Login', 'Password', '');
  Info.EmailAddress := F.ReadString(SmtpRegister + 'Addresses', 'Email', '');
  Info.ReplyAddress := F.ReadString(SmtpRegister + 'Addresses', 'Answer', '');
  F.Free;
end;

procedure SetMailOutParams(Host, Port: string; Auth: TAuthenticationType; Id,
  Pw: string);
begin
  Info.SmtpServer := Host;
  Info.SmtpPort := Port;
  Info.SmtpUseLogin := Auth;
  Info.SmtpAccountName := Id;
  Info.SmtpPassword := Pw;
end;

procedure SetMailOutAddresses(From, Answer: string);
begin
  Info.EmailAddress := From;
  Info.ReplyAddress := Answer;
end;

function SendMail(Subject, Towards: string;
                   Body: TStrings;
                   CC: string = ''; BCC: string = '';
                   Attachments: TStrings = nil): Boolean;
var
  Mess: TIdMessage;
  IdSMTP1: TIdSMTP;
  I: Integer;
begin
  Result := false;

  IdSMTP1 := TIdSMTP.Create(nil);
  try
    IdSMTP1.host := Info.SmtpServer;
    IdSMTP1.port := strtoint(Info.SmtpPort);
    IdSMTP1.AuthenticationType := Info.SmtpUseLogin;
    IdSMTP1.Username := Info.SmtpAccountName;
    IdSMTP1.Password := Info.SmtpPassword;
    IdSMTP1.MailAgent := 'Indy 9.0.11';
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

      //Mess.from.Name := Info.EmailAddress;
      Mess.from.Address := Info.EmailAddress;
      Mess.ReplyTo.Emailaddresses := Info.ReplyAddress; // comma separated addresses

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

function SendMail(Subject, Towards, Body: string;
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

procedure Recall_Default_MailOut_Params_and_Addresses;
begin
  ReadSmtp; // get values from the registry
end;

begin
  ReadSmtp; // get values from the registry
end.
