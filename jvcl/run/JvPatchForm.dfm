object PatchFrm: TPatchFrm
  Left = 419
  Top = 183
  BorderStyle = bsDialog
  Caption = 'Patcher Editor'
  ClientHeight = 144
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 4
    Top = 2
    Width = 392
    Height = 97
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object Label1: TLabel
      Left = 9
      Top = 18
      Width = 37
      Height = 13
      Caption = '&Source:'
      FocusControl = edSource
    end
    object Label2: TLabel
      Left = 9
      Top = 44
      Width = 56
      Height = 13
      Caption = '&Destination:'
      FocusControl = edDest
    end
    object Label3: TLabel
      Left = 9
      Top = 70
      Width = 49
      Height = 13
      Caption = '&Password:'
      FocusControl = edPassword
    end
    object edPassword: TEdit
      Left = 68
      Top = 66
      Width = 315
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
    object edSource: TEdit
      Left = 68
      Top = 14
      Width = 285
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edDest: TEdit
      Left = 68
      Top = 40
      Width = 285
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object btnSrc: TButton
      Left = 360
      Top = 16
      Width = 21
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnSrcClick
    end
    object btnDest: TButton
      Left = 360
      Top = 40
      Width = 21
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = '...'
      TabOrder = 3
      OnClick = btnDestClick
    end
  end
  object OkBtn: TButton
    Left = 223
    Top = 110
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 307
    Top = 110
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ClearBtn: TButton
    Left = 16
    Top = 112
    Width = 75
    Height = 25
    Caption = '&Clear'
    ModalResult = 1
    TabOrder = 1
    OnClick = ClearBtnClick
  end
end
