unit uConst;

interface

const
  // Errors
  cFilterLoadError = '������ ��� �������� DirectShow �������';
  cEnumMediaTypesError = '���������� �������� ������ MediaTypes. Camera, Filter ��� Graph �������� nil (������ %d)';
  cNoMediaTypes = '�� �������� �� ������ MediaType �� IPin (������ %d)';
  cUnableToGetEnumMediaTypes = '���������� �������� EnumMediaTypes: IEnumMediaTypes (������ %d)';
  cUnableToGetCaptureVideoPin = '���������� �������� Capture-Video-Pin � ��������� ������ (������ %d)';
  cCameraIndexOutOfRange = '���������� ������� ������ � �������� %d';
  cMediaTypeIndexOutOfRange = '���������� ������� MediaType � �������� %d';
  cApplyMediaTypeError = '�� ������� ��������� ��������� MediaType ��� ������ %d. ��� ������: %s';
  cUnableQueryInterface = '���������� ��������� ��������� %s ��� ������ %d';
  cFilterIsNull = '���������� ������ nil (������ %d)';
  cStartVideoError = '������ ��� ������� ������ ����� (������ %d). ��� ������: %s';
  cStartCaptureNotReadyError = '���������� �� ������ � ������� �����';
  cSynchronizeNoneModeError = '����������� ������ ������������� �����. ���������� ���������� ��������';
  cSaveWithoutPoints = '�� ����� ���������� ��������� �� ��������� �� ����� �����. ���������� ��������� ���������.';

  cOutOfRangeCoordinate = 'Out of range. %d is not in range [0, %d] OR %d is not in range [0, %d]';
  cOutOfRangeWrappers = 'Out of range. %d is not in range [0, %d]';  

implementation

end.
