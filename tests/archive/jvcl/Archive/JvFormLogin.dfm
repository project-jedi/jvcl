object FormLogi: TFormLogi
  Left = 457
  Top = 339
  ActiveControl = EdValue1
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Enter Login'
  ClientHeight = 149
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 10
    Width = 36
    Height = 13
    Caption = 'Value&1:'
    FocusControl = EdValue1
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 33
    Height = 13
    Caption = 'Value&2'
    FocusControl = EdValue2
  end
  object EdValue1: TJvEdit
    Left = 8
    Top = 27
    Width = 281
    Height = 21
    GroupIndex = -1
    MaxPixel.Font.Charset = DEFAULT_CHARSET
    MaxPixel.Font.Color = clWindowText
    MaxPixel.Font.Height = -11
    MaxPixel.Font.Name = 'MS Sans Serif'
    MaxPixel.Font.Style = []
    Modified = False
    SelStart = 0
    SelLength = 0
    Anchors = [akLeft, akTop, akRight]
    Ctl3D = True
    ParentCtl3D = False
    PasswordChar = #0
    ReadOnly = False
    TabOrder = 0
  end
  object EdValue2: TJvEdit
    Left = 8
    Top = 72
    Width = 281
    Height = 21
    GroupIndex = -1
    MaxPixel.Font.Charset = DEFAULT_CHARSET
    MaxPixel.Font.Color = clWindowText
    MaxPixel.Font.Height = -11
    MaxPixel.Font.Name = 'MS Sans Serif'
    MaxPixel.Font.Style = []
    Modified = False
    SelStart = 0
    SelLength = 0
    Anchors = [akLeft, akTop, akRight]
    Ctl3D = True
    ParentCtl3D = False
    PasswordChar = #0
    ReadOnly = False
    TabOrder = 1
  end
  object BtnOK: TButton
    Left = 128
    Top = 114
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object BtnCancel: TButton
    Left = 216
    Top = 114
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
