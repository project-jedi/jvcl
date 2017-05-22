object WatchForm: TWatchForm
  Left = 261
  Top = 223
  Width = 264
  Height = 219
  BorderStyle = bsSizeToolWin
  Caption = 'Watch'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = [fsBold]
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 256
    Height = 192
    Align = alClient
    BevelOuter = bvLowered
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Shape1: TShape
      Left = 1
      Top = 1
      Width = 254
      Height = 169
      Align = alClient
    end
    object TabSet1: TTabSet
      Left = 1
      Top = 170
      Width = 254
      Height = 21
      Align = alBottom
      Font.Charset = GB2312_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      SelectedColor = clWindow
      Tabs.Strings = (
        'Watch1'
        'Watch2'
        'Watch3'
        'Watch4')
      TabIndex = 0
      UnselectedColor = clBtnFace
    end
    object ListView1: TListView
      Left = 1
      Top = 1
      Width = 254
      Height = 169
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
      TabOrder = 1
      ViewStyle = vsReport
      OnResize = ListView1Resize
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
    Left = 104
    Top = 96
  end
end
