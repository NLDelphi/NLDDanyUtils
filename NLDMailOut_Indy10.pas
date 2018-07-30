unit NLDMailOut_Indy10;

// Dany Rosseel

// Version for Indy 10

{ History of this unit
28-10-2003: * Initial version
20-03-2005: * Converted the "SendMail" procedures into functions. They
              return True (success) or false (failiure)
20-04-2005: * Small correction in "SendMail", made "word" variable an "integer".
29-07-2018: * Adapted for Indy 10, see also "http://www.indyproject.org/Sockets/Blogs/RLebeau/2005_08_17_A.EN.aspx"
}

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  Classes,
  IdSMTP,
  IdAttachmentFile,
  IdText;

procedure SetMailOutParams(Host, Port: string; Auth: TIdSMTPAuthenticationType; Id,
  Pw: string);

procedure SetMailOutAddresses(From, Answer: string);

procedure Recall_Default_MailOut_Params_and_Addresses;

function SendMail(Subject, Towards: string; Body_: TStrings;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
overload;

function SendMail(Subject, Towards: string; Body_: string;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
overload;

function SendMail(Subject: string; Towards: Tstrings; Body_: TStrings;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
overload;

function SendMail(Subject: string; Towards: Tstrings; Body_: string;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
overload;

implementation

uses
  SysUtils,
  IdMessage,
  NLDRcsStrings,
  NLDSimpleRegistry,
  NLDSMTP_Register;

var
  SMTPHost: string;
  SMTPPort: string;
  SMTPAuth: TIdSMTPAuthenticationType;
  SMTPId: string;
  SMTPPw: string;
  SMTPFrom: string;
  SMTPAnswer: string;

procedure ReadSmtp;
var
  F: TSimpleRegistry;
begin
  F := TSimpleRegistry.Create;
  SMTPHost := F.ReadString(SmtpRegister + 'Configuration', 'Server', '');
  SMTPPort := F.ReadString(SmtpRegister + 'Configuration', 'Port', '25');
  SMTPAuth := TIdSMTPAuthenticationType(F.ReadInteger(SmtpRegister + 'Login', 'UseLogin', Integer(satNone)));
  SMTPId := F.ReadString(SmtpRegister + 'Login', 'AccountName', '');
  SMTPPw := F.ReadString(SmtpRegister + 'Login', 'Password', '');
  SMTPFrom := F.ReadString(SmtpRegister + 'Addresses', 'Email', '');
  SMTPAnswer := F.ReadString(SmtpRegister + 'Addresses', 'Answer', '');
  F.Free;
end;

procedure SetMailOutParams(Host, Port: string; Auth: TIdSMTPAuthenticationType; Id,
  Pw: string);
begin
  SMTPHost := Host;
  SMTPPort := Port;
  SMTPAuth := Auth;
  SMTPId := Id;
  SMTPPw := Pw;
end;

procedure SetMailOutAddresses(From, Answer: string);
begin
  SMTPFrom := From;
  SMTPAnswer := Answer;
end;

const
  ContentTypes: array[0..8] of array[0..1] of string =
  (
    ('.txt', 'text/plain'),
    ('.html', 'text/html'),
    ('.htm', 'text/html'),
    ('.jpeg', 'image/jpeg'),
    ('.jpg', 'image/jpeg'),
    ('.zip', 'application/x-zip-compressed'),
    ('.gif', 'image/gif'),
    ('.png', 'image/png'),
    ('.rar', 'application/x-rar-compressed')
    );

function SendMail(Subject, Towards: string; Body_: TStrings;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
var
  Mess: TIdMessage;
  IdSMTP1: TIdSMTP;
  I, J: Integer;
  Ext_: string;
begin
  Result := false;

  IdSMTP1 := TIdSMTP.Create(nil);
  try
    IdSMTP1.host := SMTPHost;
    IdSMTP1.port := strtoint(SMTPPort);
    IdSMTP1.AuthType := SMTPAuth;
    IdSMTP1.Username := SMTPId;
    IdSMTP1.Password := SMTPPw;
    IdSMTP1.MailAgent := 'Indy 10';
    IdSMTP1.ReadTimeout := 60000;

    Mess := TIdMessage.Create(nil);
    try
      Mess.Encoding := meMIME;
      Mess.AttachmentEncoding := 'MIME';
      Mess.ContentType := 'multipart/alternative';
      Mess.Charset := 'iso-8859-1';
      Mess.ContentTransferEncoding := '7bit';

      Mess.Subject := Subject;

      Mess.Recipients.Emailaddresses := Towards; // comma separated addresses
      Mess.CCList.EMailAddresses := CC; // comma separated addresses
      Mess.BCCList.EMailAddresses := BCC; // comma separated addresses

      Mess.from.Address := SMTPFrom;
      Mess.ReplyTo.Emailaddresses := SMTPAnswer; // comma separated addresses      

      if not Assigned(Attachments) then // no attachments
      begin
        with TIdText.Create(Mess.MessageParts, nil) do
        begin
          Body.Text := Body_.Text;
          ContentType := 'text/plain';
        end;
      end

      else // attachments present
      begin
        // Add a blank TIdText with its ContentType property set to multipart/alternative
        with TIdText.Create(mess.MessageParts, nil) do
        begin
          ContentType := 'multipart/alternative';
          //  Leave the ParentPart properties set to -1
        end;

        // Add TIdText instances for the plain-text and HTML contents. Set the ParentPart properties to the index of the multipart/altrnative part.
        with TIdText.Create(Mess.MessageParts, nil) do
        begin
          Body.Text := Body_.Text;
          ContentType := 'text/plain';
          ParentPart := 0;
        end;

        for I := 0 to Attachments.Count - 1 do
        begin
          if FileExists(Attachments[I]) then
          begin
            with TIdAttachmentFile.Create(Mess.MessageParts, Attachments[I]) do
            begin
              ContentType := 'application/octet-stream'; // default (binary) contenttype

              // find out the actual contenttype
              Ext_ := ExtractFileExt(Attachments[I]);
              for J := 0 to High(ContentTypes) do
              begin
                if Uppercase(Ext_) = Uppercase(ContentTypes[J, 0]) then
                begin
                  ContentType := ContentTypes[J, 1];
                  break;
                end;
              end;

              FileName := ExtractFileName(Attachments[I]);
            end;
          end;
        end;
        Mess.ContentType := 'multipart/mixed';
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
        on Exception do ; // failiure
      end;

    finally
      Mess.Free;
    end;

  finally
    IdSMTP1.Free;
  end;
end;

function SendMail(Subject, Towards: string; Body_: string;
  CC: string = ''; BCC: string = ''; Attachments: TStrings = nil): Boolean;
var
  Bdy: TStrings;
begin
  Bdy := TStringList.Create;
  try
    Bdy.Add(Body_);
    Result := Sendmail(Subject, Towards, Bdy, CC, BCC, Attachments);
  finally
    Bdy.Free;
  end;
end;

function SendMail(Subject: string; Towards: Tstrings; Body_: TStrings;
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
  Result := SendMail(Subject, TStringsToString(Towards), Body_, CC, BCC, Attachments);
end;

function SendMail(Subject: string; Towards: Tstrings; Body_: string;
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
  Result := SendMail(Subject, TStringsToString(Towards), Body_, CC, BCC, Attachments);
end;

procedure Recall_Default_MailOut_Params_and_Addresses;
begin
  ReadSmtp; // get values from the registry
end;

begin
  ReadSmtp; // get values from the registry
end.
