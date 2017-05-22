object frmTest: TfrmTest
  Left = 272
  Top = 180
  AutoScroll = False
  Caption = 'Test Form'
  ClientHeight = 536
  ClientWidth = 615
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 150
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PanelForLabel: TPanel
    Left = 0
    Top = 0
    Width = 615
    Height = 141
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblTest: TLabel
      Left = 4
      Top = 4
      Width = 21
      Height = 13
      Caption = 'Test'
    end
    object Edit1: TEdit
      Left = 0
      Top = 120
      Width = 615
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 0
      Text = 'Edit1'
      OnChange = Edit1Change1
    end
  end
  object mmChanges: TMemo
    Left = 0
    Top = 141
    Width = 615
    Height = 395
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
