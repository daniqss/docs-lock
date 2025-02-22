import { useQuery, useMutation } from "@tanstack/react-query";
import {
  createSection,
  deleteSection,
  getSection,
  getSections,
} from "../client/services/sections";
import { CreateSectionRequest, Section } from "../__generated__/types.gen";

export const useGetSections = () => {
  return useQuery<Section[], Error>({
    queryKey: ["sections"],
    queryFn: getSections,
  });
};

export const useGetSection = (sectionId: string) => {
  return useQuery<Section, Error>({
    queryKey: ["section", sectionId],
    queryFn: async () => await getSection(sectionId),
  });
};

export const useCreateSection = () => {
  return useMutation({
    mutationFn: (newSection: CreateSectionRequest) => {
      return Promise.resolve(createSection(newSection));
    },
  });
};

export const useDeleteSection = () => {
  return useMutation({
    mutationFn: (sectionId: string) => {
      return Promise.resolve(deleteSection(sectionId));
    },
  });
};
