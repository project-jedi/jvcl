object zWizardForm: TzWizardForm
  Left = 292
  Top = 223
  Anchors = [akRight, akBottom]
  BorderStyle = bsDialog
  Caption = 'Band Object DLL Project Wizard'
  ClientHeight = 289
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 305
    Height = 233
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 125
    Height = 13
    Caption = 'Band &Name (e.g. MyBand)'
    FocusControl = EditBandName
  end
  object Label2: TLabel
    Left = 24
    Top = 64
    Width = 131
    Height = 13
    Caption = '&Description (e.g. &&My Band)'
    FocusControl = EditBandDesc
  end
  object Button1: TButton
    Left = 72
    Top = 256
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 152
    Top = 256
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object EditBandName: TEdit
    Left = 24
    Top = 32
    Width = 273
    Height = 21
    TabOrder = 0
  end
  object RgBandType: TRadioGroup
    Left = 24
    Top = 112
    Width = 273
    Height = 113
    Caption = ' Band &Type '
    ItemIndex = 0
    Items.Strings = (
      'IE Info Band (Vertical)'
      'IE Comm Band (Horizontal)'
      'IE Tool Band'
      'Desk Band')
    TabOrder = 2
  end
  object EditBandDesc: TEdit
    Left = 24
    Top = 80
    Width = 273
    Height = 21
    TabOrder = 1
  end
  object Button3: TButton
    Left = 232
    Top = 256
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 5
    OnClick = Button3Click
  end
end
