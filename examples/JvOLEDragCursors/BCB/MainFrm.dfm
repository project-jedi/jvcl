object Form1: TForm1
  Left = 264
  Top = 253
  Width = 469
  Height = 374
  Caption = 'LoadOLEDragCursors Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 461
    Height = 291
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel7: TPanel
      Left = 0
      Top = 246
      Width = 461
      Height = 43
      Cursor = 7
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crMultiDragLinkAlt'
      TabOrder = 0
    end
    object Panel6: TPanel
      Left = 0
      Top = 205
      Width = 461
      Height = 41
      Cursor = 6
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crMultiDragAlt'
      TabOrder = 1
    end
    object Panel5: TPanel
      Left = 0
      Top = 164
      Width = 461
      Height = 41
      Cursor = 5
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crDragAlt'
      TabOrder = 2
    end
    object Panel4: TPanel
      Left = 0
      Top = 123
      Width = 461
      Height = 41
      Cursor = 4
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crMultiDragLink'
      TabOrder = 3
    end
    object Panel3: TPanel
      Left = 0
      Top = 82
      Width = 461
      Height = 41
      Cursor = crMultiDrag
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crMultiDrag'
      TabOrder = 4
    end
    object Panel2: TPanel
      Left = 0
      Top = 41
      Width = 461
      Height = 41
      Cursor = crDrag
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crDrag'
      TabOrder = 5
    end
    object Panel9: TPanel
      Left = 0
      Top = 0
      Width = 461
      Height = 41
      Cursor = crNoDrop
      Anchors = [akLeft, akTop, akRight]
      Caption = 'crNoDrop'
      TabOrder = 6
    end
  end
  object Panel8: TPanel
    Left = 0
    Top = 291
    Width = 461
    Height = 56
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
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
end
