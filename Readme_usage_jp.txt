%================================================================
%      Readme of ���艷�ʖʈȉ��̊��C���ʉ�̓c�[��
%================================================================
1.�����
  Linux + intel fortran (ifort 10.1)�Ŋm�F�BLinux + �x�m�ʃR���p�C��(f90)�ł��m�F�B

2.�f�B���N�g���\��
 ${isen_cam}/
	     src/  �\�[�X�R�[�h
	     Doc/  �h�L�������g
	�@�@ work/ grads��ctl�t�@�C���������Ă���B
	�@�@ Makefile   ���C�N�t�@�C��
   	     Readme_namelist_eng.txt�@namelist�̐���
	     sample.sh�@�@�T���v���f�[�^�p�̃V�F���X�N���v�g

3.���̓f�[�^
�@grads�`���̃f�[�^����́B�@�@
�@�Eu-wind
�@�Ev-wind
�@�Egeopotential height
�@�Eair temperature
�@�Esurface pressure
  �Esurface u-wind
�@�Esurface v-wind
�@�Esurface temperature
  �Esurface altitude
 surface pressure��sea level pressure�ł���p�B

4.�g�p���@
�@4.1 �R���p�C��
    �͂��߂ɁA${isen_cam}/Makefile �̎g�p����R���p�C�����`�F�b�N����B
    $cd ${isen_cam}/
    $make
    ��������΁Aisen_cold_air���쐬�����B

�@4.2 �l�[�����X�g�̍쐬
�@�@sample.sh���Q�l�ɂ���namelist���쐬�B
   namelist�̏ڍׂ�Readme_namelist_eng.txt���Q�ƁB

�@4.3 ���
    �쐬�����l�[�����X�g(namelist)���g���ꍇ�A
�@�@$./isen_cold_air < namelist

  GrADS�p��ctl�t�@�C����work/isen.ctl��
  �C�����Ďg�p���Ă��������B
�@��̓I�Ȍv�Z�ɂ��Ă̓h�L�������g(${isen_cam}/Doc/document.pdf)���������������B


5.���ӓ_

�n�\�t�߂̋t�]�w�́A�����Ɍ��ĉ��ʂ��ŏ��ƂȂ�_�̒l���g���ď������Ă���B
���艷�ʂ͑Η������`���w��z�肵�ăv���O�������쐬���Ă��邽�߁A
�Η����E�ʕt�߂ɂł���t�]�w�ɂ��Ă͍l�����Ă��Ȃ��B

���艷�ʖʂ̋C���A���x�A���C�e�ʂƂ��̃t���b�N�X�A��������
namelist�̐ݒ�ŏo�͂����Ȃ����Ƃ��\�B
�o�̓t�@�C���̃T�C�Y�����炷���Ƃ͂ł��邪�A�v�Z�͂���Ă���B

���̓t�@�C���̓��A����̊��Ԃ������v�Z�������ꍇ
(�Ⴆ�Γ��̓t�@�C����1�N����1���������v�Z�������Ƃ�)�A
namelist��INITIAL_TIME�AEND_TIME�����܂��g���Ē������ĉ������B

1�O��time step�̏�񂪖����ꍇ�A���l�����ʂ͊��C���ʂ̔��U�����Ōv�Z����邱�Ƃɒ��ӁB
INPUT_TDEF_DAYNUM�͎��ԕω����̕�������肷��Ƃ��ɕK�v�B




