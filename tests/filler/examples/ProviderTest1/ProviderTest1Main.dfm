object frmTestProviders: TfrmTestProviders
  Left = 296
  Top = 146
  Width = 642
  Height = 497
  Caption = 'frmTestProviders'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object JvProvidedLabel1: TJvProvidedLabel
    Left = 35
    Top = 125
    Width = 94
    Height = 13
    Caption = 'JvProvidedLabel1'
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    Provider = JvFontDataProvider1
    Index = 0
  end
  object JvProvidedListBox1: TJvProvidedListBox
    Left = 35
    Top = 20
    Width = 121
    Height = 97
    Provider = JvFontDataProvider1
    ItemHeight = 16
    TabOrder = 0
  end
  object JvProvidedListBox2: TJvProvidedListBox
    Left = 180
    Top = 20
    Width = 121
    Height = 97
    Provider = JvTreeDataProvider1
    ItemHeight = 16
    TabOrder = 1
  end
  object JvFontDataProvider1: TJvFontDataProvider
    Left = 345
    Top = 40
  end
  object JvTreeDataProvider1: TJvTreeDataProvider
    Left = 375
    Top = 105
    Root = <
      item
        Implementers = <
          item
            ClassName = 'TJvTreeDataItemsManagement'
          end
          item
            ClassName = 'TJvTreeDataItemsDesigner'
          end
          item
            ClassName = 'TJvCustomDataItemsImages'
          end>
        Items = <
          item
            ClassName = 'TJvBaseDataItem'
            ID = 'BB8C38AD7179CC4F92627DC8FD3BAF'
            Implementers = <
              item
                ClassName = 'TJvDataItemTextImpl'
                Caption = 'Test'
              end>
            SubItems = <
              item
                ClassName = 'TJvTreeDataItems'
                Implementers = <
                  item
                    ClassName = 'TJvTreeDataItemsManagement'
                  end
                  item
                    ClassName = 'TJvTreeDataItemsDesigner'
                  end
                  item
                    ClassName = 'TJvCustomDataItemsImages'
                  end>
                Items = <
                  item
                    ClassName = 'TJvBaseDataItem'
                    ID = '5AF3487DC16A6445B900888EC38D13'
                    Implementers = <
                      item
                        ClassName = 'TJvDataItemTextImpl'
                        Caption = 'Test 1.1'
                      end
                      item
                        ClassName = 'TJvDataItemImageImpl'
                      end>
                  end
                  item
                    ClassName = 'TJvBaseDataItem'
                    ID = '6B2D64EBB9ED6F4AA843EABE67934A'
                    Implementers = <
                      item
                        ClassName = 'TJvDataItemTextImpl'
                        Caption = 'Test 1.2'
                      end
                      item
                        ClassName = 'TJvDataItemImageImpl'
                        Alignment = taRightJustify
                        ImageIndex = 1
                        SelectedIndex = 1
                      end>
                    SubItems = <
                      item
                        ClassName = 'TJvTreeDataItems'
                        Implementers = <
                          item
                            ClassName = 'TJvTreeDataItemsManagement'
                          end
                          item
                            ClassName = 'TJvTreeDataItemsDesigner'
                          end
                          item
                            ClassName = 'TJvCustomDataItemsImages'
                          end>
                        Items = <
                          item
                            ClassName = 'TJvBaseDataItem'
                            ID = '0FD9BCEDDB71F74BA3B0A5D7DF30A4'
                            Implementers = <
                              item
                                ClassName = 'TJvDataItemTextImpl'
                                Caption = 'Test 1.2.1'
                              end>
                          end>
                      end>
                  end>
              end>
          end>
      end>
  end
end
