object PassForm: TPassForm
  Left = 513
  Top = 382
  ActiveControl = EdPassword
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Enter Password'
  ClientHeight = 87
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 9
    Width = 59
    Height = 13
    Caption = '&Password:'
    FocusControl = EdPassword
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object EdPassword: TJvEdit
    Left = 8
    Top = 26
    Width = 313
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
  object BtnOK: TButton
    Left = 160
    Top = 56
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&OK'
    Default = True
    TabOrder = 1
  end
  object BtnCancel: TButton
    Left = 248
    Top = 56
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
