import { Section, CreateSectionRequest } from "../../__generated__";
import client from "../client";

export const createSection = (createSectionRequest: CreateSectionRequest) =>
  client
    .post("/sections", createSectionRequest)
    .then((response) => response.data);

export const deleteSection = (sectionId: string) => {
  client.delete(`/sections/${sectionId}`).then((response) => response.data);
};

export const getSection = (sectionId: string) =>
  client
    .get<Section>(`/sections/${sectionId}`)
    .then((response) => response.data);

export const getSections = async (): Promise<Section[]> => {
  const response = await client.get<Section[]>("/sections");
  return response.data;
};
