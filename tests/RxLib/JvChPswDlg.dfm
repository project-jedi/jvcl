object ChPswdForm: TChPswdForm
  Left = 309
  Top = 149
  ActiveControl = OldPswd
  BorderIcons = []
  BorderStyle = bsDialog
  ClientHeight = 115
  ClientWidth = 347
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OldPswdLabel: TLabel
    Left = 19
    Top = 20
    Width = 94
    Height = 13
    AutoSize = False
  end
  object NewPswdLabel: TLabel
    Left = 19
    Top = 52
    Width = 94
    Height = 13
    AutoSize = False
  end
  object ConfirmLabel: TLabel
    Left = 19
    Top = 84
    Width = 94
    Height = 13
    AutoSize = False
  end
  object OldPswd: TEdit
    Left = 116
    Top = 16
    Width = 117
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
    OnChange = PswdChange
  end
  object NewPswd: TEdit
    Left = 116
    Top = 48
    Width = 117
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
    OnChange = PswdChange
  end
  object ConfirmNewPswd: TEdit
    Left = 116
    Top = 80
    Width = 117
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
    OnChange = PswdChange
  end
  object OkBtn: TButton
    Left = 254
    Top = 16
    Width = 77
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 254
    Top = 48
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
