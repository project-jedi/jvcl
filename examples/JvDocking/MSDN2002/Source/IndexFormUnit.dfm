object IndexForm: TIndexForm
  Left = 256
  Top = 134
  Width = 236
  Height = 270
  BorderStyle = bsSizeToolWin
  Caption = 'Index'
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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 228
    Height = 111
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    object Label1: TLabel
      Left = 0
      Top = 3
      Width = 47
      Height = 15
      Caption = '&Look for:'
    end
    object Label2: TLabel
      Left = 0
      Top = 55
      Width = 59
      Height = 15
      Caption = 'F&iltered by:'
    end
    object LookforComboBox: TComboBox
      Left = 0
      Top = 24
      Width = 281
      Height = 23
      ItemHeight = 15
      TabOrder = 0
    end
    object FilteredbyComboBox: TComboBox
      Left = 0
      Top = 79
      Width = 281
      Height = 23
      Style = csDropDownList
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
  end
  object ListBox1: TListBox
    Left = 0
    Top = 111
    Width = 228
    Height = 132
    Align = alClient
    ItemHeight = 15
    TabOrder = 1
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 150
    TBDockHeight = 150
    NCPopupMenu = MSDN2002.PopupMenu1
    DirectDrag = False
    ShowHint = True
    EnableCloseButton= True
    DockStyle = MSDN2002.JvDockVSNetStyle1
    Left = 40
    Top = 128
  end
end
