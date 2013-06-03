object JvClipboardViewerFrm: TJvClipboardViewerFrm
  Left = 0
  Top = 0
  Width = 557
  Height = 460
  TabOrder = 0
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 40
    Top = 3
    Width = 505
    Height = 446
    Buttons = [capClose, capHelp]
    Caption = 'a clipboard Viewer with JvClipboardViewer'
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'Arial'
    CaptionFont.Style = [fsBold]
    TabOrder = 0
    object Label1: TLabel
      Left = 219
      Top = 4
      Width = 78
      Height = 13
      Caption = 'Clipboard as text'
    end
    object Image1: TImage
      Left = 82
      Top = 140
      Width = 355
      Height = 295
      Stretch = True
    end
    object Label2: TLabel
      Left = 115
      Top = 120
      Width = 287
      Height = 13
      Caption = 'Clipboard as image (Try the print screen/Alt+print screen key)'
    end
    object Memo1: TMemo
      Left = 82
      Top = 22
      Width = 355
      Height = 89
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object JvClipboardViewer1: TJvClipboardViewer
    OnImage = JvClipboardViewer1Image
    OnText = JvClipboardViewer1Text
    Left = 154
    Top = 112
  end
end
