object Form1: TForm1
  Left = 544
  Top = 277
  BorderStyle = bsSingle
  Caption = 'LoadOLEDragCursors Demo'
  ClientHeight = 348
  ClientWidth = 460
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel8: TPanel
    Left = 0
    Top = 292
    Width = 460
    Height = 56
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 171
      Top = 12
      Width = 103
      Height = 25
      Anchors = [akBottom]
      Caption = 'Load OLE Cursors'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 460
    Height = 292
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Panel7: TPanel
      Left = 0
      Top = 246
      Width = 460
      Height = 43
      Cursor = 7
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crMultiDragLinkAlt'
      TabOrder = 0
    end
    object Panel6: TPanel
      Left = 0
      Top = 205
      Width = 460
      Height = 41
      Cursor = 6
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crMultiDragAlt'
      TabOrder = 1
    end
    object Panel5: TPanel
      Left = 0
      Top = 164
      Width = 460
      Height = 41
      Cursor = 5
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crDragAlt'
      TabOrder = 2
    end
    object Panel4: TPanel
      Left = 0
      Top = 123
      Width = 460
      Height = 41
      Cursor = 4
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crMultiDragLink'
      TabOrder = 3
    end
    object Panel3: TPanel
      Left = 0
      Top = 82
      Width = 460
      Height = 41
      Cursor = crMultiDrag
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crMultiDrag'
      TabOrder = 4
    end
    object Panel2: TPanel
      Left = 0
      Top = 41
      Width = 460
      Height = 41
      Cursor = crDrag
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crDrag'
      TabOrder = 5
    end
    object Panel9: TPanel
      Left = 0
      Top = 0
      Width = 460
      Height = 41
      Cursor = crNoDrop
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crNoDrop'
      TabOrder = 6
    end
  end
end
