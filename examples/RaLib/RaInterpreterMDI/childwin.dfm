object MDIChild: TMDIChild
  Left = 197
  Top = 117
  Width = 290
  Height = 207
  Caption = 'MDI Child'
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsMDIChild
  OldCreateOrder = True
  Position = poDefault
  Visible = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 282
    Height = 177
    Align = alClient
    Lines.Strings = (
      'This Window is loaded from chlidwin.pas/dfm.')
    TabOrder = 0
  end
end
