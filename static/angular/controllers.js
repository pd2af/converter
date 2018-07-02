angular
  .module('upload_xml', ['ngFileUpload'])
  // .controller("AppController", ['$scope', 'FileUploader', function($scope, FileUploader) {
  //   var uploader = $scope.uploader = new FileUploader({
  //     url: '/translator.rkt'
  //   });
  .controller("AppController", ['$scope', 'Upload', function($scope, Upload) {

    $scope.delayed = {};
    $scope.download_button = false;
    $scope.file_selector_button_text = "Select file";

    $scope.refresh_filename = function(file) {
      $scope.translate_button = true;
      $scope.file_selector_button_text = file.name;
      $scope.addClass("is-info");
    }

    $scope.submit = function() {
      var pd_file = $scope.form.file;
      $scope.filename = $scope.file.name;
      if ($scope.file) {
        $scope.upload($scope.file);
      }
    }

    $scope.upload = function(file) {
      Upload.upload({
          url: '/translator',
          data: {'file': file, 'action': 'parse', 'filename': $scope.filename}
      }).then(function(response) {
          var result = response.data;
          $scope.af_filename = result.af_filename;
          $scope.af_fileurl = result.af_fileurl;
          if ($scope.af_filename) {
            $scope.download_button = true;
          }
          else {
            $scope.download_button = false;
          }
      }, function (response) {
          console.log('Error status: ' + response.status);
      }, function (evt) {
            var progressPercentage = parseInt(100.0 * evt.loaded / evt.total);
            // console.log('progress: ' + progressPercentage + '% ' + evt.config.data.file.name);
      });
    }

    $scope.reset = function() {
      $scope.download_button = false;
      $scope.translate_button = false;
      $scope.file_selector_button_text = "Select file";
      var el = angular.element(document.querySelector("#file_selector"));
      el.addClass("is-info");
      // $scope.ResultPD = "";
      // $scope.ResultAF = "";
      // $scope.ResultAF_XML = "";
    }
  }]);
