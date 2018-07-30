object SMTPSetupForm: TSMTPSetupForm
  Left = 237
  Top = 193
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'SMTP Setup'
  ClientHeight = 341
  ClientWidth = 460
  Color = clBtnFace
  Constraints.MinHeight = 366
  Constraints.MinWidth = 464
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 16
    Top = 16
    Width = 361
    Height = 201
    Caption = ' Uitgaande E-mail server '
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 32
      Width = 73
      Height = 13
      Caption = 'Server (SMTP):'
    end
    object Label2: TLabel
      Left = 16
      Top = 64
      Width = 28
      Height = 13
      Caption = 'Poort:'
    end
    object Label3: TLabel
      Left = 16
      Top = 136
      Width = 75
      Height = 13
      Caption = 'Account naam: '
    end
    object Label4: TLabel
      Left = 16
      Top = 168
      Width = 64
      Height = 13
      Caption = 'Wachtwoord:'
    end
    object SmtpServerEdit: TEdit
      Left = 112
      Top = 32
      Width = 225
      Height = 21
      TabOrder = 0
      OnChange = SmtpServerEditChange
    end
    object SmtpPortEdit: TEdit
      Left = 112
      Top = 64
      Width = 73
      Height = 21
      TabOrder = 1
      OnChange = SmtpPortEditChange
    end
    object UsedCheckBox: TCheckBox
      Left = 16
      Top = 104
      Width = 137
      Height = 17
      Caption = 'Server login nodig'
      TabOrder = 2
      OnClick = UsedCheckBoxClick
    end
    object SmtpAccountNameEdit: TEdit
      Left = 112
      Top = 136
      Width = 225
      Height = 21
      TabOrder = 3
      OnChange = SmtpAccountNameEditChange
    end
    object SmtpPasswordEdit: TEdit
      Left = 112
      Top = 168
      Width = 225
      Height = 21
      TabOrder = 4
      OnChange = SmtpPasswordEditChange
    end
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 224
    Width = 361
    Height = 105
    Caption = ' Adressen '
    TabOrder = 1
    object Label5: TLabel
      Left = 16
      Top = 24
      Width = 61
      Height = 13
      Caption = 'E-mail Adres:'
    end
    object Label6: TLabel
      Left = 16
      Top = 64
      Width = 81
      Height = 13
      Caption = 'Antwoord Adres: '
    end
    object EmailAddressEdit: TEdit
      Left = 112
      Top = 24
      Width = 225
      Height = 21
      TabOrder = 0
      OnChange = EmailAddressEditChange
    end
    object ReplyAddressEdit: TEdit
      Left = 112
      Top = 64
      Width = 225
      Height = 21
      TabOrder = 1
      OnChange = ReplyAddressEditChange
    end
  end
  object Button1: TButton
    Left = 384
    Top = 304
    Width = 65
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
    OnClick = Button1Click
  end
end
