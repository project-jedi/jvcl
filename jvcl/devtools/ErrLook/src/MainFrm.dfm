object frmMain: TfrmMain
  Left = 367
  Top = 150
  ActiveControl = edValue
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Error Lookup'
  ClientHeight = 222
  ClientWidth = 278
  Color = clBtnFace
  Constraints.MinHeight = 155
  Constraints.MinWidth = 275
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = false
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHelp = FormHelp
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 47
    Width = 69
    Height = 13
    Caption = 'Error Message'
  end
  object lblHex: TLabel
    Left = 268
    Top = 8
    Width = 3
    Height = 13
    Cursor = crHandPoint
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = lblHexClick
  end
  object lblInt: TLabel
    Left = 268
    Top = 24
    Width = 3
    Height = 13
    Cursor = crHandPoint
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = lblIntClick
  end
  object Label2: TLabel
    Left = 16
    Top = 12
    Width = 30
    Height = 13
    Caption = '&Value:'
    FocusControl = edValue
  end
  object edValue: TEdit
    Left = 64
    Top = 8
    Width = 73
    Height = 21
    TabOrder = 0
    Text = '0'
  end
  object reErrMsg: TRichEdit
    Left = 5
    Top = 64
    Width = 266
    Height = 130
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    PlainText = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WantReturns = False
  end
  object btnModules: TButton
    Left = 8
    Top = 199
    Width = 60
    Height = 21
    Action = acModules
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object btnLookup: TButton
    Left = 72
    Top = 199
    Width = 60
    Height = 21
    Action = acLookUp
    Anchors = [akLeft, akBottom]
    Default = True
    TabOrder = 3
  end
  object btnClose: TButton
    Left = 136
    Top = 199
    Width = 60
    Height = 21
    Action = acClose
    Anchors = [akLeft, akBottom]
    TabOrder = 4
  end
  object btnHelp: TButton
    Left = 200
    Top = 199
    Width = 60
    Height = 21
    Action = acHelp
    Anchors = [akLeft, akBottom]
    TabOrder = 5
  end
  object UpDown1: TUpDown
    Left = 137
    Top = 8
    Width = 16
    Height = 21
    Associate = edValue
    Min = -32768
    Max = 32767
    Position = 0
    TabOrder = 6
    Thousands = False
    Wrap = False
  end
  object alMain: TActionList
    Left = 192
    Top = 24
    object acModules: TAction
      Caption = '&Modules...'
      ShortCut = 16461
      OnExecute = acModulesExecute
    end
    object acLookUp: TAction
      Caption = '&Look Up'
      ShortCut = 13
      OnExecute = acLookUpExecute
    end
    object acClose: TAction
      Caption = 'Clo&se'
      ShortCut = 27
      OnExecute = acCloseExecute
    end
    object acHelp: TAction
      Caption = '&Help'
      ShortCut = 112
      OnExecute = acHelpExecute
    end
  end
end
