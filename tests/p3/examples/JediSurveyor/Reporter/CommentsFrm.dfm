object frmComments: TfrmComments
  Left = 484
  Top = 184
  Width = 412
  Height = 381
  ActiveControl = reComments
  Caption = 'Comments on "%s"'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object reComments: TJvRichEdit
    Left = 6
    Top = 6
    Width = 386
    Height = 306
    Anchors = [akLeft, akTop, akRight, akBottom]
    PlainText = True
    ReadOnly = True
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 307
    Top = 323
    Width = 75
    Height = 25
    Action = acClose
    Anchors = [akRight, akBottom]
    TabOrder = 1
  end
  object alCommentsFrm: TActionList
    Left = 222
    Top = 72
    object acClose: TAction
      Caption = '&Close'
      ShortCut = 27
      OnExecute = acCloseExecute
    end
  end
end
