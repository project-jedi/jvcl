object SearchForm: TSearchForm
  Left = 653
  Top = 220
  Width = 234
  Height = 322
  BorderStyle = bsSizeToolWin
  Caption = 'Search'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'ËÎÌו'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 226
    Height = 295
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    object Label1: TLabel
      Left = 0
      Top = 2
      Width = 47
      Height = 15
      Caption = '&Look for:'
    end
    object Label2: TLabel
      Left = 0
      Top = 44
      Width = 59
      Height = 15
      Caption = 'F&iltered by:'
    end
    object LookforComboBox: TComboBox
      Left = 0
      Top = 19
      Width = 225
      Height = 23
      ItemHeight = 15
      TabOrder = 0
    end
    object FilteredbyComboBox: TComboBox
      Left = 0
      Top = 63
      Width = 225
      Height = 23
      ItemHeight = 15
      TabOrder = 1
      Items.Strings = (
        '(no filter)'
        '.NET Framework SDK'
        'Enterprise Servers'
        'Internet Development'
        'Platform SDK'
        'Samples'
        'Visual Basic'
        'Visual Basic and Related'
        'Visual C#'
        'Visual C# and Related'
        'Visual C++'
        'Visual C++ and Related'
        'Visual Studio'
        'Visual Studio and Related'
        'Visual Studio Macros')
    end
    object SearchButton: TButton
      Left = 0
      Top = 96
      Width = 75
      Height = 25
      Caption = '&Search'
      Default = True
      TabOrder = 2
    end
    object SearchintitlesonlyCheckBox: TCheckBox
      Left = 0
      Top = 128
      Width = 153
      Height = 17
      Caption = 'Search in &titles only'
      TabOrder = 3
    end
    object MatchrelatedwordsCheckBox: TCheckBox
      Left = 0
      Top = 152
      Width = 137
      Height = 17
      Caption = 'Match &related words'
      TabOrder = 4
    end
    object SearchinpreviousresultsCheckBox: TCheckBox
      Left = 0
      Top = 176
      Width = 177
      Height = 17
      Caption = 'Search in &previous results'
      TabOrder = 5
    end
    object HighlightsearchhitsCheckBox: TCheckBox
      Left = 0
      Top = 202
      Width = 217
      Height = 17
      Caption = 'Hi&ghlight search hits(in topics)'
      TabOrder = 6
    end
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 100
    TBDockHeight = 100
    NCPopupMenu = MSDN2002.PopupMenu1
    DirectDrag = False
    ShowHint = True
    EnableCloseButton = True
    DockStyle = MSDN2002.JvDockVSNetStyle1
    Left = 128
    Top = 88
  end
end
