object PassForm: TPassForm
  Left = 513
  Top = 382
  ActiveControl = edPassword
  BorderStyle = bsDialog
  Caption = 'Enter Password'
  ClientHeight = 128
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
    Top = 17
    Width = 49
    Height = 13
    Caption = '&Password:'
    FocusControl = edPassword
  end
  object edPassword: TJvEdit
    Left = 8
    Top = 34
    Width = 313
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    Modified = False
    MaxPixel.Font.Charset = DEFAULT_CHARSET
    MaxPixel.Font.Color = clWindowText
    MaxPixel.Font.Height = -11
    MaxPixel.Font.Name = 'MS Sans Serif'
    MaxPixel.Font.Style = []
    SelStart = 0
    SelLength = 0
  end
  object btnOK: TButton
    Left = 160
    Top = 91
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 240
    Top = 91
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
