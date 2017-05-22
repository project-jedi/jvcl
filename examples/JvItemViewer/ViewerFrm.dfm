object frmImageViewer: TfrmImageViewer
  Left = 328
  Top = 112
  Width = 562
  Height = 454
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Image Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 408
    Width = 554
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 554
    Height = 408
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    Color = clWindow
    ParentColor = False
    PopupMenu = PopupMenu1
    TabOrder = 1
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 225
      Height = 201
      AutoSize = True
      Center = True
    end
  end
  object ActionList1: TActionList
    Left = 56
    Top = 32
    object acFullScreen: TAction
      Caption = 'Full Screen'
      ShortCut = 122
      OnExecute = acFullScreenExecute
    end
    object acClose: TAction
      Caption = 'Close'
      ShortCut = 27
      OnExecute = acCloseExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 144
    Top = 32
    object FullScreen1: TMenuItem
      Action = acFullScreen
    end
    object Close1: TMenuItem
      Action = acClose
    end
  end
end
