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
  TextHeight = 12
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 228
    Height = 89
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    object Label1: TLabel
      Left = 0
      Top = 2
      Width = 54
      Height = 12
      Caption = '&Look for:'
    end
    object Label2: TLabel
      Left = 0
      Top = 44
      Width = 72
      Height = 12
      Caption = 'F&iltered by:'
    end
    object LookforComboBox: TComboBox
      Left = 0
      Top = 19
      Width = 225
      Height = 20
      ItemHeight = 12
      TabOrder = 0
    end
    object FilteredbyComboBox: TComboBox
      Left = 0
      Top = 63
      Width = 225
      Height = 20
      Style = csDropDownList
      ItemHeight = 12
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
    Top = 89
    Width = 228
    Height = 154
    Align = alClient
    ItemHeight = 12
    TabOrder = 1
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 150
    TBDockHeight = 150
    NCPopupMenu = MSDN2002.PopupMenu1
    DirectDrag = False
    ShowHint = True
    DockStyle = MSDN2002.JvDockVSNetStyle1
    Left = 40
    Top = 128
  end
end
