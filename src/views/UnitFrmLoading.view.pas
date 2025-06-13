unit UnitFrmLoading.view;

interface

uses
  System.Classes, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  ACBrGIF, Vcl.Imaging.GIFImg;

type
  TFormLoading = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  FormLoading: TFormLoading;

implementation

{$R *.dfm}


procedure TFormLoading.FormCreate(Sender: TObject);
begin
	TGIFImage(Image1.Picture.Graphic).Animate := true;
end;

end.
