object frmMain: TfrmMain
  Left = 251
  Top = 110
  Width = 620
  Height = 400
  Caption = 'JvPreviewDocument Demo'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 620
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 270
    Width = 612
    Height = 65
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 44
      Height = 13
      Caption = 'Columns:'
    end
    object Label2: TLabel
      Left = 56
      Top = 8
      Width = 30
      Height = 13
      Caption = 'Rows:'
    end
    object Label3: TLabel
      Left = 104
      Top = 8
      Width = 42
      Height = 13
      Caption = 'Shadow:'
    end
    object Label4: TLabel
      Left = 152
      Top = 8
      Width = 29
      Height = 13
      Caption = 'Scale:'
    end
    object Label5: TLabel
      Left = 480
      Top = 8
      Width = 42
      Height = 13
      Caption = 'Preview:'
    end
    object Label6: TLabel
      Left = 344
      Top = 8
      Width = 58
      Height = 13
      Caption = 'Scale Mode:'
    end
    object Label7: TLabel
      Left = 216
      Top = 8
      Width = 24
      Height = 13
      Caption = 'Vert:'
    end
    object Label8: TLabel
      Left = 280
      Top = 8
      Width = 26
      Height = 13
      Caption = 'Horz:'
    end
    object edCols: TEdit
      Left = 8
      Top = 24
      Width = 25
      Height = 21
      TabOrder = 0
      Text = '1'
    end
    object udCols: TUpDown
      Left = 33
      Top = 24
      Width = 15
      Height = 21
      Associate = edCols
      Min = 1
      Position = 1
      TabOrder = 1
      Wrap = False
      OnClick = udColsClick
    end
    object edRows: TEdit
      Left = 56
      Top = 24
      Width = 25
      Height = 21
      TabOrder = 2
      Text = '1'
    end
    object udRows: TUpDown
      Left = 81
      Top = 24
      Width = 15
      Height = 21
      Associate = edRows
      Min = 1
      Position = 1
      TabOrder = 3
      Wrap = False
      OnClick = udRowsClick
    end
    object edShadow: TEdit
      Left = 104
      Top = 24
      Width = 25
      Height = 21
      TabOrder = 4
      Text = '4'
    end
    object udShadowWidth: TUpDown
      Left = 129
      Top = 24
      Width = 15
      Height = 21
      Associate = edShadow
      Min = -100
      Position = 4
      TabOrder = 5
      Wrap = False
      OnClick = udShadowWidthClick
    end
    object edScale: TEdit
      Left = 152
      Top = 24
      Width = 41
      Height = 21
      TabOrder = 6
      Text = '100'
    end
    object udZoom: TUpDown
      Left = 193
      Top = 24
      Width = 16
      Height = 21
      Associate = edScale
      Min = 1
      Max = 500
      Position = 100
      TabOrder = 7
      Thousands = False
      Wrap = False
      OnClick = udZoomClick
    end
    object cbPreview: TComboBox
      Left = 480
      Top = 24
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 8
      OnChange = cbPreviewChange
      Items.Strings = (
        'Screen'
        'Printer')
    end
    object cbScaleMode: TComboBox
      Left = 344
      Top = 24
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 9
      OnChange = cbScaleModeChange
      Items.Strings = (
        'Full Page'
        'Page Width'
        'Use Scale'
        'Use Auto Scale'
        'Use Cols and Rows')
    end
    object edVert: TEdit
      Left = 216
      Top = 24
      Width = 41
      Height = 21
      TabOrder = 10
      Text = '8'
    end
    object udVertSpacing: TUpDown
      Left = 257
      Top = 24
      Width = 15
      Height = 21
      Associate = edVert
      Min = 0
      Max = 500
      Position = 8
      TabOrder = 11
      Thousands = False
      Wrap = False
      OnClick = udVertSpacingClick
    end
    object edHorz: TEdit
      Left = 280
      Top = 24
      Width = 41
      Height = 21
      TabOrder = 12
      Text = '8'
    end
    object udHorzSpacing: TUpDown
      Left = 321
      Top = 24
      Width = 15
      Height = 21
      Associate = edHorz
      Min = 0
      Max = 500
      Position = 8
      TabOrder = 13
      Thousands = False
      Wrap = False
      OnClick = udHorzSpacingClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 612
    Height = 270
    ActivePage = tabOriginal
    Align = alClient
    TabOrder = 1
    object tabOriginal: TTabSheet
      Caption = 'Original'
      ImageIndex = 1
      object reOriginal: TJvRichEdit
        Left = 0
        Top = 0
        Width = 604
        Height = 242
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        WantTabs = True
        Zoom = 100
      end
    end
    object tabPreview: TTabSheet
      Caption = 'Preview'
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 335
    Width = 612
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 150
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 182
    Top = 64
  end
  object MainMenu1: TMainMenu
    Left = 96
    Top = 64
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = 'Print...'
        ShortCut = 16464
        OnClick = Print1Click
      end
      object Printer1: TMenuItem
        Caption = 'Printer Settings...'
        ShortCut = 24656
        OnClick = Printer1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        ShortCut = 32883
        OnClick = Exit1Click
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object First1: TMenuItem
        Caption = 'First'
        ShortCut = 16420
        OnClick = First1Click
      end
      object Previous1: TMenuItem
        Caption = 'Previous'
        ShortCut = 16417
        OnClick = Previous1Click
      end
      object Next1: TMenuItem
        Caption = 'Next'
        ShortCut = 16418
        OnClick = Next1Click
      end
      object Last1: TMenuItem
        Caption = 'Last'
        ShortCut = 16419
        OnClick = Last1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuPreview: TMenuItem
        Caption = 'Preview'
        object Control1: TMenuItem
          Caption = 'Control'
          OnClick = Control1Click
        end
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object mnuMargins: TMenuItem
        Caption = 'Margins'
        Checked = True
        OnClick = mnuMarginsClick
      end
      object Clear1: TMenuItem
        Caption = 'Clear'
        OnClick = Clear1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Caption = 'About...'
        OnClick = About1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'RTF files|*.rtf|Text files|*.txt'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 92
    Top = 112
  end
  object PrintDialog1: TPrintDialog
    Copies = 1
    MaxPage = 33
    Options = [poPageNums]
    PrintRange = prPageNums
    ToPage = 33
    Left = 94
    Top = 160
  end
end
