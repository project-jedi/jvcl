object FormLogi: TFormLogi
  Left = 457
  Top = 339
  ActiveControl = edUserName
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
  DesignSize = (
    298
    149)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 10
    Width = 51
    Height = 13
    Caption = '&Username:'
    FocusControl = edUserName
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 49
    Height = 13
    Caption = '&Password:'
    FocusControl = edPassword
  end
  object edUserName: TJvEdit
    Left = 8
    Top = 27
    Width = 281
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
  object edPassword: TJvEdit
    Left = 8
    Top = 72
    Width = 281
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
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
    Left = 120
    Top = 114
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 200
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
