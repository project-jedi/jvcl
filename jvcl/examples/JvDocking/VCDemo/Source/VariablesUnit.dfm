object VariablesForm: TVariablesForm
  Left = 312
  Top = 252
  Width = 235
  Height = 239
  BorderStyle = bsSizeToolWin
  Caption = 'Variables'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 227
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    object Label1: TLabel
      Left = 4
      Top = 7
      Width = 48
      Height = 13
      Caption = 'Context:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ComboBox1: TComboBox
      Left = 64
      Top = 4
      Width = 161
      Height = 21
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 33
    Width = 227
    Height = 179
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Panel2'
    TabOrder = 1
    object ListView1: TListView
      Left = 0
      Top = 0
      Width = 223
      Height = 154
      Align = alClient
      BorderStyle = bsNone
      Columns = <
        item
          Caption = 'Name'
        end
        item
          Caption = 'Value'
        end>
      GridLines = True
      TabOrder = 0
      ViewStyle = vsReport
      OnResize = ListView1Resize
    end
    object TabSet1: TTabSet
      Left = 0
      Top = 154
      Width = 223
      Height = 21
      Align = alBottom
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      SelectedColor = clWindow
      Tabs.Strings = (
        'Auto'
        'Locals'
        'this')
      TabIndex = 0
      UnselectedColor = clBtnFace
    end
  end
  object lbDockClient1: TJvDockClient
    OnFormShow = lbDockClient1FormShow
    OnFormHide = lbDockClient1FormHide
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = True
    ShowHint = True
    EnableCloseButton = True
    EachOtherDock = False
    DockStyle = MainForm.JvDockVCStyle1
    Left = 136
    Top = 104
  end
end
