object frmMain: TfrmMain
  Left = 237
  Top = 125
  Width = 445
  Height = 523
  Caption = 'JvSimpleXML Demo (test XML encoding and decoding)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 266
    Width = 437
    Height = 230
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      437
      230)
    object Label2: TLabel
      Left = 6
      Top = 8
      Width = 33
      Height = 13
      Caption = '&Result:'
      FocusControl = reResult
    end
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 437
      Height = 5
      Align = alTop
      Shape = bsTopLine
    end
    object reResult: TMemo
      Left = 5
      Top = 26
      Width = 425
      Height = 137
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object btnCopy: TButton
      Left = 300
      Top = 177
      Width = 117
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Copy To Source'
      TabOrder = 1
      OnClick = btnCopyClick
    end
    object StatusBar1: TStatusBar
      Left = 0
      Top = 211
      Width = 437
      Height = 19
      Panels = <
        item
          Width = 50
        end>
      SimplePanel = False
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 437
    Height = 266
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      437
      266)
    object Label1: TLabel
      Left = 6
      Top = 12
      Width = 37
      Height = 13
      Caption = '&Source:'
      FocusControl = reSource
    end
    object reSource: TMemo
      Left = 5
      Top = 28
      Width = 425
      Height = 178
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'Put the xml in this memo. Click the Encode/Decode '
        'buttons to see the result in the bottom memo. '
        ''
        'You can drag and drop files from the Explorer into '
        'the program and have them automatically opened.'
        ''
        'If you drag multiple files, they will be all be added to'
        'the memo.')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object chkTrim: TCheckBox
      Left = 11
      Top = 220
      Width = 80
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Trim &Blanks'
      TabOrder = 1
    end
    object btnEncode: TButton
      Left = 258
      Top = 226
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Encode'
      TabOrder = 4
      OnClick = btnEncodeClick
    end
    object btnDecode: TButton
      Left = 342
      Top = 226
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Decode'
      TabOrder = 5
      OnClick = btnDecodeClick
    end
    object chkUseUTF8: TCheckBox
      Left = 106
      Top = 220
      Width = 133
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Use UTF&8 functions'
      TabOrder = 2
    end
    object chkUseClipboard: TCheckBox
      Left = 11
      Top = 240
      Width = 229
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = '&Use clipboard for text transfers'
      TabOrder = 3
    end
  end
end
