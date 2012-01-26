/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.text.NumberFormat;
import java.util.ArrayList;

/**
 * 
 * @author Deno
 */
public class Torrents {

	String fileName, size, status, dspeed, upspeed, seeders, leechers,
			downloaded, uploaded;
	int id, type;
	long down, up, totalSize;
	double statusD;

	public Torrents(int i) {
		down = 0;
		up = 0;
		totalSize = 1;
		statusD = 0;
		type = 1;
		id = i;
		fileName = "";
		size = "1";
		status = "";
		dspeed = "";
		upspeed = "";
		seeders = "";
		leechers = "";
		downloaded = "0";
		uploaded = "0";

	}

	public void setType(int i) {
		type = i;
	}

	public void setFileName(String s) {
		fileName = s;
	}

	public void setSize(String s) {
		size = s;
		totalSize = Long.parseLong(size);
	}

	public void setStatus(String s) {

		status = s;
	}

	public void setDspeed(String s) {
		dspeed = s;
	}

	public void setUpspeed(String s) {
		upspeed = s;
	}

	public void setSeeders(String s) {
		seeders = s;
	}

	public void setLeechers(String s) {
		leechers = s;
	}

	public void setDownloaded(String s) {
		NumberFormat r = NumberFormat.getInstance();
		r.setMaximumFractionDigits(1);
		r.setMinimumFractionDigits(1);
		down = down + Long.parseLong(s);
		statusD = (Double.parseDouble(Long.toString(down)) / Double
				.parseDouble(Long.toString(totalSize))) * 100;
		setStatus(r.format(statusD) + "%");
		downloaded = Long.toString(down);
	}

	public void setUploaded(String s) {
		up = up + Long.parseLong(s);
		uploaded = Long.toString(up);
	}

	public int getType() {
		return type;
	}

	public String getFileName() {
		return fileName;
	}

	public String getSize() {
		return setSizeFormat(size);
	}

	public String getStatus() {

		return status;
	}

	public String getDspeed() {
		return dspeed;
	}

	public String getUpspeed() {
		return upspeed;
	}

	public String getSeeders() {
		return seeders;
	}

	public String getLeechers() {
		return leechers;
	}

	public String getDownloaded() {
		return setSizeFormat(downloaded);
	}

	public String getUploaded() {
		return setSizeFormat(uploaded);
	}

	public int getId() {
		return id;
	}

	public String setSizeFormat(String i) {

		long checkSize = Long.parseLong(i);
		long checkGb = 1073741824L;
		long checkMb = 1048576;
		long checkKb = 1024L;
		double sizeS = Double.parseDouble(i);
		double gb = 1073741824;
		double mb = 1048576;
		double kb = 1024;
		NumberFormat r = NumberFormat.getInstance();
		r.setMinimumFractionDigits(2);
		r.setMaximumFractionDigits(2);

		if (checkSize / checkGb > 0) {
			return r.format(sizeS / gb) + " GB";
		} else if (checkSize / checkMb > 0) {
			return r.format(sizeS / mb) + " MB";
		} else if (checkSize / checkKb > 0) {
			return r.format(sizeS / kb) + " KB";
		}

		return Double.toString(sizeS) + " B";
	}

}
