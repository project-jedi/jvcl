object OutputForm: TOutputForm
  Left = 397
  Top = 269
  Width = 347
  Height = 202
  BorderStyle = bsSizeToolWin
  Caption = 'Output'
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
    Width = 339
    Height = 175
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Panel1'
    TabOrder = 0
    object Panel2: TPanel
      Left = 0
      Top = 154
      Width = 335
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      OnCanResize = Panel2CanResize
      object Splitter1: TSplitter
        Left = 241
        Top = 0
        Width = 3
        Height = 17
        Cursor = crHSplit
      end
      object TabSet1: TTabSet
        Left = 0
        Top = 0
        Width = 241
        Height = 17
        Align = alLeft
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        SelectedColor = clWindow
        Tabs.Strings = (
          'Build'
          'Debug'
          'Find in Files 1'
          'Find in Files 2'
          'Results'
          'SQL Debugging')
        TabIndex = 0
        UnselectedColor = clBtnFace
      end
      object ScrollBar1: TScrollBar
        Left = 244
        Top = 0
        Width = 91
        Height = 17
        Align = alClient
        PageSize = 0
        TabOrder = 1
      end
    end
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 335
      Height = 154
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
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
    Left = 64
    Top = 56
  end
end
