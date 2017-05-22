object Form1: TForm1
  Left = 152
  Top = 85
  Width = 568
  Height = 367
  Caption = 'JvStringGrid Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvSg: TJvStringGrid
    Left = 0
    Top = 0
    Width = 293
    Height = 340
    Align = alClient
    ColCount = 2
    FixedCols = 0
    RowCount = 2
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goRowMoving, goColMoving, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking]
    TabOrder = 0
    Alignment = taLeftJustify
    FixedFont.Charset = DEFAULT_CHARSET
    FixedFont.Color = clWindowText
    FixedFont.Height = -11
    FixedFont.Name = 'MS Shell Dlg 2'
    FixedFont.Style = []
  end
  object Panel1: TPanel
    Left = 293
    Top = 0
    Width = 267
    Height = 340
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 24
      Top = 24
      Width = 22
      Height = 13
      Caption = 'Row'
    end
    object Label2: TLabel
      Left = 136
      Top = 24
      Width = 15
      Height = 13
      Caption = 'Col'
    end
    object Label3: TLabel
      Left = 24
      Top = 232
      Width = 23
      Height = 13
      Caption = 'Data'
    end
    object edRow: TEdit
      Left = 24
      Top = 40
      Width = 65
      Height = 21
      TabOrder = 4
      Text = '0'
    end
    object udRows: TUpDown
      Left = 89
      Top = 40
      Width = 15
      Height = 21
      Associate = edRow
      Min = 0
      Position = 0
      TabOrder = 5
      Wrap = False
    end
    object edCol: TEdit
      Left = 136
      Top = 40
      Width = 65
      Height = 21
      TabOrder = 6
      Text = '0'
    end
    object udCols: TUpDown
      Left = 201
      Top = 40
      Width = 15
      Height = 21
      Associate = edCol
      Min = 0
      Position = 0
      TabOrder = 7
      Wrap = False
    end
    object RowInsert: TButton
      Left = 24
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Insert'
      TabOrder = 8
      OnClick = RowInsertClick
    end
    object RowDelete: TButton
      Left = 24
      Top = 112
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 9
      OnClick = RowDeleteClick
    end
    object RowHide: TButton
      Left = 24
      Top = 144
      Width = 75
      Height = 25
      Caption = 'Hide'
      TabOrder = 10
      OnClick = RowHideClick
    end
    object RowShow: TButton
      Left = 24
      Top = 176
      Width = 75
      Height = 25
      Caption = 'Show'
      TabOrder = 11
      OnClick = RowShowClick
    end
    object ColInsert: TButton
      Left = 136
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Insert'
      TabOrder = 12
      OnClick = ColInsertClick
    end
    object ColDelete: TButton
      Left = 136
      Top = 112
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 13
      OnClick = ColDeleteClick
    end
    object ColHide: TButton
      Left = 136
      Top = 144
      Width = 75
      Height = 25
      Caption = 'Hide'
      TabOrder = 14
      OnClick = ColHideClick
    end
    object ColShow: TButton
      Left = 136
      Top = 176
      Width = 75
      Height = 25
      Caption = 'Show'
      TabOrder = 15
      OnClick = ColShowClick
    end
    object reData: TRichEdit
      Left = 24
      Top = 248
      Width = 225
      Height = 78
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        'The content of this memo will be inserted into '
        'the stringgrid when you click either of the '
        '"Insert" buttons.')
      PlainText = True
      ScrollBars = ssBoth
      TabOrder = 16
      WordWrap = False
    end
    object Clear: TButton
      Left = 87
      Top = 210
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 17
      OnClick = ClearClick
    end
    object btnAddRow: TButton
      Left = 22
      Top = 3
      Width = 17
      Height = 17
      Caption = '+'
      TabOrder = 0
      OnClick = btnAddRowClick
    end
    object btnDelRow: TButton
      Left = 46
      Top = 3
      Width = 17
      Height = 17
      Caption = '-'
      TabOrder = 1
      OnClick = btnDelRowClick
    end
    object btnAddCol: TButton
      Left = 134
      Top = 3
      Width = 17
      Height = 17
      Caption = '+'
      TabOrder = 2
      OnClick = btnAddColClick
    end
    object btnDelCol: TButton
      Left = 158
      Top = 3
      Width = 17
      Height = 17
      Caption = '-'
      TabOrder = 3
      OnClick = btnDelColClick
    end
  end
end
